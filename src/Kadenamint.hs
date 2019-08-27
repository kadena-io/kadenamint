{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Kadenamint where

import Control.Concurrent                               (threadDelay)
import Control.Concurrent.Async                         (withAsync)
import Control.Lens                                     (strict, view, (&), (.~))
import Control.Monad                                    (when)
import Control.Monad.Except                             (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), asks, runReaderT)
import Control.Monad.State                              (MonadState(..), StateT(..), evalStateT, get, modify)
import qualified Control.Monad.State.Strict as Strict
import Data.Binary.Builder                              (toLazyByteString)
import Data.Bool                                        (bool)
import Data.ByteArray.Encoding                          (Base(Base16), convertToBase)
import Data.Colour.SRGB                                 (Colour, sRGB24)
import Data.Conduit.Network                             (HostPreference, ServerSettings, serverSettings)
import Data.Default                                     (Default(..))
import Data.Foldable                                    (for_)
import Data.String                                      (IsString)
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import Network.HTTP.Types                               (encodePath)
import Shelly                                           (Sh, shelly, silently, run_)
import qualified Shelly as Sh
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..), setSGRCode)
import Text.Read as T                                   (readMaybe)

import Prelude                                          hiding (log)

import Data.ByteArray.HexString                         (HexString(..))
import qualified Data.ByteArray.HexString as Hex
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger     (mkLogStdout)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.Repl as Pact
import qualified Pact.Repl.Types as Pact

import System.Which

{- Process orchestration -}
type ActorEffects m = (MonadIO m, MonadReader Env m, MonadState Int m)

data Actor
  = Actor_Node
  | Actor_Broadcast
  deriving (Eq, Ord, Enum, Bounded, Show)

runEverything :: IO ()
runEverything =
  withAsync (runActor nodeEnv Actor_Node) $ \_ ->
    withAsync (runActor broadcastEnv Actor_Broadcast) $ \_ ->
      runABCI abciEnv

timelineEntries :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineEntries =
  let
    deleteNetwork = run_ "rm" ["-rf", tendermintHome]
    initNetwork = tendermint "init" []

    launchNode = shelly tendermintNode
    resetNetwork = do
      shelly deleteNetwork
      shelly initNetwork
      log "Node has been reset"
  in
    [ (seconds 0, Actor_Node, resetNetwork)
    , (seconds 0, Actor_Node, launchNode)

    , (seconds 1, Actor_Broadcast, broadcastPactTransaction greetWorld)
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction "(greet-world.set-message \"hello\")")
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction "(greet-world.greet)")
    ]

greetWorld :: Text
greetWorld = [here|
(module greet-world MODULE_ADMIN
  "A smart contract to greet the world."

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () true)

  (defschema message-schema
    @doc "Message schema"
    @model [(invariant (!= msg ""))]

    msg:string)

  (deftable
    message:{message-schema})

  (defun set-message
    (
      m:string
    )
    "Set the message that will be used next"
    ; uncomment the following to make the model happy!
    ; (enforce (!= m "") "set-message: must not be empty")
    (write message "0" {"msg": m})
  )

  (defun greet ()
    "Do the hello-world dance"
    (with-default-read message "0" { "msg": "" } { "msg":= msg }
      (format "Hello {}!" [msg])))
)

(create-table message)

(set-message "world")
(greet)
|]

runActor :: Env -> Actor -> IO ()
runActor env actor = flip runReaderT env $ flip evalStateT 0 $
  for_ timelineEntries $ \(entryDelay, entryActor, entryAction) ->
    when (actor == entryActor) $ do
      liftIO $ threadDelay entryDelay
      entryAction

data PactTransaction = PactTransaction
  { _pactTransaction_nonce :: Int
  , _pactTransaction_code  :: Text
  } deriving (Eq, Ord, Read, Show)

broadcastPactTransaction :: ActorEffects m => Text -> m ()
broadcastPactTransaction code = do
  log $ "Broadcasting pact code:\n  " <> code
  nonce <- get
  modify (+1)
  broadcastTransaction $ tshow $ PactTransaction nonce code

broadcastTransaction :: (MonadIO m, MonadReader Env m) => Text -> m ()
broadcastTransaction t = do
  log $ "Broadcasting transaction:\n  " <> t
  let txHex = "0x" <> convertToBase Base16 (T.encodeUtf8 t)
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath ["broadcast_tx_sync"] [("tx", Just txHex)] of
    Left err -> do
      log "Failed encoding of transaction with error:"
      log $ tshow err
      fatal
    Right pathAndQuery -> do
      let url = mkAddress defaultTendermintRPCHostPort <> pathAndQuery
      log $ "Broadcasting at:\t" <> url
      shelly $ silently $ run_ "curl" [url]

runABCI :: Env -> IO ()
runABCI env = do
  rs <- Pact.initReplState Pact.StringEval Nothing
  _logger <- mkLogStdout -- too noisy

  let
    transformHandler :: EffectsT (Response t) -> IO (Response t)
    transformHandler er = do
      x <- runExceptT $ runReaderT er env
      case x of
        Right r -> pure r
        Left l -> pure $ ResponseException $ def
          & _exceptionError .~ l

  serveAppWith (mkServerSettings defaultABCIAppHostPort) mempty $ transformApp transformHandler $ app rs

{- Env -}
data Env = Env
  { _env_printer :: Text -> Text
  }

red, green, cyan :: Colour Float
red   = sRGB24 0xFF 0 0
green = sRGB24 0 0xFF 0
cyan  = sRGB24 0 0xFF 0xFF

broadcastEnv, nodeEnv, abciEnv :: Env
broadcastEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground cyan]
  }
nodeEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground red]
  }
abciEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground green]
  }

{- Tendermint -}
tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermint :: Text -> [Text] -> Sh ()
tendermint tmCmd cmdArgs = run_ tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", tendermintHome]

tendermintNode :: Sh ()
tendermintNode = tendermint "node"
  [ "--p2p.laddr", mkAddress defaultTendermintP2PHostPort
  , "--rpc.laddr", "tcp://" <> mkAddress defaultTendermintRPCHostPort & _UPSTREAM_ "incoherent with other address flags"
  , "--proxy_app", mkAddress defaultABCIAppHostPort
  ]

tendermintHome :: IsString a => a
tendermintHome = "./.tendermint"

defaultTendermintP2PHostPort :: (Text, Int)
defaultTendermintP2PHostPort = ("0.0.0.0", 26656)

defaultTendermintRPCHostPort :: (Text, Int)
defaultTendermintRPCHostPort = ("127.0.0.1", 26657)

{- ABCI app -}
defaultABCIAppHostPort :: IsString a => (a, Int)
defaultABCIAppHostPort = ("127.0.0.1", 26658)

type Err = Text
type EffectsT = ReaderT Env (ExceptT Err IO)
type MonadEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

app :: Pact.ReplState -> App EffectsT
app rs = App $ \case
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain _ -> pure def
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx (CheckTx hx) -> check rs hx
  RequestDeliverTx (DeliverTx hx) -> deliver rs hx --runPact ResponseDeliverTx _deliverTxCode False rs a
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

check :: MonadEffects m => Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check = runPact accept reject True
  where
    accept msg = do
      log msg
      pure def
    reject msg = do
      log msg
      pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

deliver :: MonadEffects m => Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver = runPact accept reject False
  where
    accept msg = do
      log msg
      pure def
    reject msg = do
      log msg
      pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1

runPact :: MonadEffects m => (Text -> m a) -> (Text -> m a) -> Bool -> Pact.ReplState -> HexString -> m a
runPact accept reject shouldRollback rs hx = do
  log $ "Decoding:\t" <> Hex.toText hx
  case decodeHexString hx of
    Left err -> reject $ "Failed decode with error: " <> tshow err
    Right p -> do
      log $ "Decoded:\t" <> p
      case T.readMaybe (T.unpack p) of
        Nothing -> reject "Failed to parse transaction"
        Just (PactTransaction _ code) -> do
          let code' = mconcat
                [ "(begin-tx)"
                , code
                , bool "(commit-tx)" "(rollback-tx)" shouldRollback
                ]
          liftIO (Strict.evalStateT (Pact.evalRepl' $ T.unpack code') rs) >>= \case
            Left err -> reject $ "Pact error:\n  " <> T.pack err
            Right r -> accept $ "Pact result:\n  " <> T.strip (tshow r)


{- Utils -}
type HostPort a = IsString a => (a, Int)

mkAddress :: HostPort Text -> Text
mkAddress (host, port) = host <> ":" <> tshow port

mkServerSettings :: HostPort HostPreference -> ServerSettings
mkServerSettings (host, port) = serverSettings port host

tshow :: Show a => a -> Text
tshow = T.pack . show

log :: (MonadIO m, MonadReader Env m) => Text -> m ()
log txt = do
  p <- asks _env_printer
  liftIO $ putStrLn $ T.unpack $ p txt

sgrify :: [SGR] -> Text -> Text
sgrify codes txt = mconcat
  [ T.pack $ setSGRCode codes
  , txt
  , T.pack $ setSGRCode [Reset]
  ]

seconds :: Int -> Int
seconds = (*1e6)

fatal :: m ()
fatal = error "fatal error"

doubleQuotes :: (IsString a, Semigroup a) => a -> a
doubleQuotes t = "\"" <> t <> "\""

decodeHexString :: HexString -> Either T.UnicodeException Text
decodeHexString (HexString bs) = T.decodeUtf8' bs & _TODO_ "make sure this is the right decoding"

{- Issue tracking -}
_UPSTREAM_ :: Text -> a -> a
_UPSTREAM_ _ = id

_TODO_ :: Text -> a -> a
_TODO_ _ = id
