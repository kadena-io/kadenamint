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
import Data.Functor                                     (void)
import Data.String                                      (IsString)
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import Network.HTTP.Types                               (encodePath)
import Shelly                                           (Sh, shelly, silently, run, run_)
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
runEverything = do
  let
    deleteNetwork = run_ "rm" ["-rf", _networkFlags_output mkNetworkFlags]

    resetNetwork = void $ do
      shelly deleteNetwork
      shelly $ tendermintNetwork mkNetworkFlags
      --log ("Network has been reset") Nothing

    g = mkGlobalFlags

  resetNetwork

  peers <- flip runReaderT nodeEnv $ do
    n0 <- shelly $ silently $ tendermintNodeId (g 0)
    n1 <- shelly $ silently $ tendermintNodeId (g 1)
    pure [(0, T.strip n0), (1, T.strip n1)]

  let
    n = mkNodeFlags peers

    launchNode i = void $ do
      log ("Node " <> tshow i <> " will be launched") Nothing
      void $ shelly $ tendermintNode (mkGlobalFlags i) (n i)
      log ("Node " <> tshow i <> " has been launched") Nothing

    go i f = flip runReaderT (nodeEnv i) $ do
      liftIO $ threadDelay $ seconds i
      f

  withAsync (runActor broadcastEnv Actor_Broadcast) $ \_ ->
    withAsync (go 0 $ launchNode 0) $ \_ ->
      withAsync (go 1 $ launchNode 1) $ \_ ->
        withAsync (runABCI 0 (n 0)) $ \_ ->
          runABCI 1 (n 1)

timelineEntries :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineEntries =
    [ (seconds 3, Actor_Broadcast, broadcastPactTransaction 0 greetWorld)
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction 0 "(greet-world.set-message \"hello\")")
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction 0 "(greet-world.greet)")
    ]

timelineRepl :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineRepl =
  [ (seconds 3, Actor_Broadcast, broadcastPactTransaction 0 "(+ 1 2)")
  , (seconds 2, Actor_Broadcast, broadcastPactTransaction 1 "(+ 2 3)")
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

broadcastPactTransaction :: ActorEffects m => Int -> Text -> m ()
broadcastPactTransaction i code = do
  let rpc = mkRPCAddress i
  log ("Broadcasting pact code to node #" <> tshow i <> " at " <> rpc) (Just code)
  nonce <- get
  modify (+1)
  broadcastTransaction rpc $ tshow $ PactTransaction nonce code

broadcastTransaction :: (MonadIO m, MonadReader Env m) => Text -> Text -> m ()
broadcastTransaction addr t = do
  let txHex = "0x" <> convertToBase Base16 (T.encodeUtf8 t)
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath ["broadcast_tx_sync"] [("tx", Just txHex)] of
    Left err -> do
      log "Failed encoding of transaction with error" (Just $ tshow err)
      fatal
    Right pathAndQuery -> do
      let url = addr <> pathAndQuery
      log "Broadcasting at" (Just url)
      shelly $ silently $ run_ "curl" [url]

runABCI :: Int -> NodeFlags -> IO ()
runABCI nid nf = do
  rs <- Pact.initReplState Pact.StringEval Nothing
  _logger <- mkLogStdout -- too noisy

  let
    env = abciEnv nid

    transformHandler :: EffectsT (Response t) -> IO (Response t)
    transformHandler er = do
      x <- runExceptT $ runReaderT er env
      case x of
        Right r -> pure r
        Left l -> pure $ ResponseException $ def
          & _exceptionError .~ l

  serveAppWith (mkServerSettings $ _nodeFlags_app nf) mempty
    $ transformApp transformHandler
    $ app nid rs

{- Env -}
newtype Env = Env
  { _env_printer :: Text -> Text
  }

red, green, cyan :: Colour Float
red   = sRGB24 0xFF 0 0
green = sRGB24 0 0xFF 0
cyan  = sRGB24 0 0xFF 0xFF

broadcastEnv :: Env
broadcastEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground cyan] . ("\n[RPC] " <>)
  }

nodeEnv, abciEnv :: Int -> Env
nodeEnv nid = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground red] $ "\n[NODE] Node: " <> tshow nid <> " " <> x
  }
abciEnv nid = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground green] $ "\n[ABCI] Node: " <> tshow nid <> " " <> x
  }

{- Tendermint -}
type Address = Text
type Peer = (Text, Address)

data GlobalFlags = GlobalFlags
  { _globalFlags_home :: Text
  }

data NetworkFlags = NetworkFlags
  { _networkFlags_validators    :: Int
  , _networkFlags_output        :: Text
  , _networkFlags_populatePeers :: Bool
  }

data NodeFlags = NodeFlags
  { _nodeFlags_global :: GlobalFlags
  , _nodeFlags_app :: forall a. HostPort a
  , _nodeFlags_p2p :: Address
  , _nodeFlags_rpc :: Address
  , _nodeFlags_peers :: [Peer]
  }

mkNetworkFlags :: NetworkFlags
mkNetworkFlags = NetworkFlags
  { _networkFlags_validators    = 2
  , _networkFlags_output        = "./.tendermint"
  , _networkFlags_populatePeers = True
  }

mkGlobalFlags :: Int -> GlobalFlags
mkGlobalFlags i = GlobalFlags { _globalFlags_home = _networkFlags_output mkNetworkFlags <> "/node" <> tshow i }

mkPortOffset :: Int -> Int
mkPortOffset = (10 *)

mkRPCAddress :: Int -> Address
mkRPCAddress i = mkAddress $ defaultTendermintRPCHostPort & _2 +~ mkPortOffset i

mkP2PAddress :: Int -> Address
mkP2PAddress i = mkAddress $ defaultTendermintP2PHostPort & _2 +~ mkPortOffset i

mkABCIHostPort :: Int -> HostPort a
mkABCIHostPort i = defaultABCIAppHostPort & _2 +~ mkPortOffset i

mkNodeFlags :: [(Int, Text)] -> Int -> NodeFlags
mkNodeFlags peers i = NodeFlags
  { _nodeFlags_global = mkGlobalFlags i
  , _nodeFlags_app = mkABCIHostPort i
  , _nodeFlags_p2p = mkP2PAddress i
  , _nodeFlags_rpc = mkRPCAddress i
  , _nodeFlags_peers = flip fmap peers $ \(i', pid) -> (pid, mkP2PAddress i')
  }

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = run tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork nf = run tendermintPath $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

tendermintNode :: GlobalFlags -> NodeFlags -> Sh Text
tendermintNode gf nf = tendermint gf "node"
  [ "--p2p.laddr", _nodeFlags_p2p nf
  , "--rpc.laddr", _nodeFlags_rpc nf & _UPSTREAM_ "incoherent with other address flags" ("tcp://" <>)
  , "--proxy_app", mkAddress $ _nodeFlags_app nf
  , "--p2p.persistent_peers", T.intercalate "," $ flip fmap (_nodeFlags_peers nf) $ \(pid, addr) -> mconcat
      [ pid
      , "@"
      , addr
      ]
  ]

tendermintNodeId :: GlobalFlags -> Sh Text
tendermintNodeId gf = tendermint gf "show_node_id" []

defaultTendermintP2PHostPort :: (Text, Int)
defaultTendermintP2PHostPort = ("127.0.0.1", 26656)

defaultTendermintRPCHostPort :: (Text, Int)
defaultTendermintRPCHostPort = ("127.0.0.1", 26657)

{- ABCI app -}
defaultABCIAppHostPort :: IsString a => (a, Int)
defaultABCIAppHostPort = ("127.0.0.1", 26658)

type Err = Text
type EffectsT = ReaderT Env (ExceptT Err IO)
type MonadEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

app :: Int -> Pact.ReplState -> App EffectsT
app nid rs = App $ \case
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain _ -> pure def
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx (CheckTx hx) -> check nid rs hx
  RequestDeliverTx (DeliverTx hx) -> deliver nid rs hx
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

check :: MonadEffects m => Int -> Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check nid = runPact nid accept reject True
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

deliver :: MonadEffects m => Int -> Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver nid = runPact nid accept reject False
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1

runPact :: MonadEffects m => nid -> m a -> m a -> Bool -> Pact.ReplState -> HexString -> m a
runPact _nid accept reject shouldRollback rs hx = do
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
