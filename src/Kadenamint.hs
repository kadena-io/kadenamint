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
import Control.Lens                                     (strict, view, _2, (&), (.~), (+~))
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
  = Actor_Node Int
  | Actor_Broadcast
  deriving (Eq, Ord, Show)

runEverything :: IO ()
runEverything = let n = mkNodeConfig in
  withAsync (runActor nodeEnv $ Actor_Node 0) $ \_ ->
    withAsync (runActor nodeEnv $ Actor_Node 1) $ \_ ->
      withAsync (runABCI (n 0) abciEnv) $ \_ ->
        withAsync (runABCI (n 1) abciEnv) $ \_ ->
          runActor broadcastEnv Actor_Broadcast

timelineEntries :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineEntries =
  let
    deleteNetwork cfg = run_ "rm" ["-rf", _nodeConfig_home cfg]
    initNetwork cfg = tendermint cfg "init" []

    launchNode cfg = shelly $ tendermintNode cfg
    resetNetwork cfg = do
      shelly $ deleteNetwork cfg
      shelly $ initNetwork cfg
      log ("Node " <> tshow (_nodeConfig_index cfg) <> " has been reset") Nothing

    n = mkNodeConfig
  in
    [ (seconds 0, Actor_Node 0, resetNetwork $ n 0)
    , (seconds 0, Actor_Node 0, launchNode $ n 0)
    , (seconds 0, Actor_Node 1, resetNetwork $ n 1)
    , (seconds 0, Actor_Node 1, launchNode $ n 1)

    , (seconds 1, Actor_Broadcast, broadcastPactTransaction (n 0) greetWorld)
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction (n 0) "(greet-world.set-message \"hello\")")
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction (n 0) "(greet-world.greet)")

    , (seconds 5, Actor_Broadcast, broadcastPactTransaction (n 1) greetWorld)
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction (n 1) "(greet-world.set-message \"hello\")")
    , (seconds 1, Actor_Broadcast, broadcastPactTransaction (n 1) "(greet-world.greet)")
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

broadcastPactTransaction :: ActorEffects m => NodeConfig -> Text -> m ()
broadcastPactTransaction cfg code = do
  log ("Broadcasting pact code to node #" <> tshow (_nodeConfig_index cfg) <> " at " <> _nodeConfig_rpc cfg) (Just code)
  nonce <- get
  modify (+1)
  broadcastTransaction cfg $ tshow $ PactTransaction nonce code

broadcastTransaction :: (MonadIO m, MonadReader Env m) => NodeConfig -> Text -> m ()
broadcastTransaction cfg t = do
  let txHex = "0x" <> convertToBase Base16 (T.encodeUtf8 t)
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath ["broadcast_tx_sync"] [("tx", Just txHex)] of
    Left err -> do
      log "Failed encoding of transaction with error" (Just $ tshow err)
      fatal
    Right pathAndQuery -> do
      let url = _nodeConfig_rpc cfg <> pathAndQuery
      log "Broadcasting at" (Just url)
      shelly $ silently $ run_ "curl" [url]

runABCI :: NodeConfig -> Env -> IO ()
runABCI cfg env = do
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

  serveAppWith (mkServerSettings $ _nodeConfig_app cfg) mempty
    $ transformApp transformHandler
    $ app rs

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
  { _env_printer = sgrify [SetRGBColor Foreground cyan] . ("\n[RPC] " <>)
  }
nodeEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground red] . ("\n[NODE] " <>)
  }
abciEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground green] . ("\n[ABCI] " <>)
  }

{- Tendermint -}
type Address = Text

data NodeConfig = NodeConfig
  { _nodeConfig_index :: Int
  , _nodeConfig_home :: Text
  , _nodeConfig_app :: forall a. IsString a => (a, Int)
  , _nodeConfig_p2p :: Address
  , _nodeConfig_rpc :: Address
  }

mkNodeConfig :: Int -> NodeConfig
mkNodeConfig i =
  let portOffset = 10 * i
  in NodeConfig
     { _nodeConfig_index = i
     , _nodeConfig_home = "./.tendermint-" <> tshow i
     , _nodeConfig_app = defaultABCIAppHostPort & _2 +~ portOffset
     , _nodeConfig_p2p = mkAddress $ defaultTendermintP2PHostPort & _2 +~ portOffset
     , _nodeConfig_rpc = mkAddress $ defaultTendermintRPCHostPort & _2 +~ portOffset
     }

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermint :: NodeConfig -> Text -> [Text] -> Sh ()
tendermint cfg tmCmd cmdArgs = run_ tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _nodeConfig_home cfg]

tendermintNode :: NodeConfig -> Sh ()
tendermintNode cfg = tendermint cfg "node"
  [ "--p2p.laddr", _nodeConfig_p2p cfg
  , "--rpc.laddr", _nodeConfig_rpc cfg & _UPSTREAM_ "incoherent with other address flags" ("tcp://" <>)
  , "--proxy_app", mkAddress $ _nodeConfig_app cfg
  ]

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
  RequestDeliverTx (DeliverTx hx) -> deliver rs hx
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

check :: MonadEffects m => Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check = runPact accept reject True
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

deliver :: MonadEffects m => Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver = runPact accept reject False
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1

runPact :: MonadEffects m => m a -> m a -> Bool -> Pact.ReplState -> HexString -> m a
runPact accept reject shouldRollback rs hx = do
  case decodeHexString hx of
    Left err -> do
      log "Failed decode with error" (Just $ tshow err)
      reject
    Right p -> do
      if shouldRollback
        then log "Checking" (Just p)
        else log "Delivering" Nothing

      case T.readMaybe (T.unpack p) of
        Nothing -> do
          log "Failed to parse transaction" Nothing
          reject
        Just (PactTransaction _ code) -> do
          let codeTx = mconcat
                [ "(begin-tx)"
                , code
                , bool "(commit-tx)" "(rollback-tx)" shouldRollback
                ]
          liftIO (Strict.evalStateT (Pact.evalRepl' $ T.unpack codeTx) rs) >>= \case
            Left err -> do
              log "Pact error" (Just $ T.pack err)
              reject
            Right r -> do
              log "Pact result" (Just $ T.strip $ tshow r)
              accept


{- Utils -}
type HostPort a = IsString a => (a, Int)

mkAddress :: HostPort Text -> Text
mkAddress (host, port) = host <> ":" <> tshow port

mkServerSettings :: HostPort HostPreference -> ServerSettings
mkServerSettings (host, port) = serverSettings port host

tshow :: Show a => a -> Text
tshow = T.pack . show

log :: (MonadIO m, MonadReader Env m) => Text -> Maybe Text -> m ()
log header body = do
  p <- asks _env_printer
  liftIO $ putStrLn $ T.unpack $ p $ header <> maybe "" (":\n" <>) body

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
