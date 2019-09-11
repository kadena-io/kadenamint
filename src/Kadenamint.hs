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
import Control.Concurrent.Async                         (forConcurrently_, withAsync)
import Control.Concurrent.MVar                          (modifyMVar_, readMVar)
import Control.Lens                                     (strict, view, _2, (&), (^.), (.~), (+~))
import Control.Monad                                    (when, (>=>))
import Control.Monad.Except                             (MonadError(..), ExceptT(..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), asks, runReaderT)
import Control.Monad.State.Strict                       (MonadState(..), StateT(..), evalStateT, get, modify)
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
import Data.Traversable                                 (for)
import Network.HTTP.Types                               (encodePath)
import Shelly                                           (Sh, shelly, silently, run, run_)
import qualified Shelly as Sh
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..), setSGRCode)
import System.Which                                     (staticWhich)
import Text.Read as T                                   (readMaybe)

import Prelude                                          hiding (log)

import Data.ByteArray.HexString                         (HexString(..))
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger     (mkLogStdout)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.PersistPactDb as Pact
import qualified Pact.Repl as Pact
import qualified Pact.Repl.Types as Pact
import qualified Pact.Types.Runtime as Pact

{- Process orchestration -}
type ActorEffects m = (MonadIO m, MonadReader Env m, MonadState Int m)

data Actor
  = Actor_Node Int
  | Actor_Broadcast
  deriving (Eq, Ord, Show)

networkSize :: Int
networkSize = 3

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

coreEnv :: Maybe Int -> Env
coreEnv nid' = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground red] $ mconcat
      [ "\n[CORE] "
      , maybe "" (\nid -> "Node: " <> tshow nid <> " | ") nid'
      , x
      ]
  }

abciEnv :: Int -> Env
abciEnv nid = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground green] $ "\n[ABCI] Node: " <> tshow nid <> " | " <> x
  }

runEverything :: IO ()
runEverything = do
  let
    deleteNetwork = run_ "rm" ["-rf", _networkFlags_output mkNetworkFlags]

    resetNetwork = do
      shelly deleteNetwork
      void $ shelly $ tendermintNetwork mkNetworkFlags
      log "Network has been reset" Nothing

    networkNodes = [0..networkSize-1]

  flip runReaderT (coreEnv Nothing) resetNetwork

  peers <- for networkNodes $ \i -> do
    ni <- shelly $ silently $ tendermintNodeId $ mkGlobalFlags i
    pure (i, T.strip ni)

  let
    launchNode i = withAsync (runABCI i) $ \_ -> do
      flip runReaderT (coreEnv $ Just i) $ do
        liftIO $ threadDelay $ seconds i `div` 10
        log ("Node " <> tshow i <> " will be launched") Nothing
        shelly $ tendermintNode (mkGlobalFlags i) (mkNodeFlags peers i)

  withAsync (runActor broadcastEnv Actor_Broadcast ) $ \_ -> do
    forConcurrently_ networkNodes launchNode

timelineEntries :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineEntries =
    [ (seconds 3, Actor_Broadcast, broadcastPactTransaction 0 greetWorld)
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction 1 "(greet-world.set-message \"hello\")")
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction 0 "(greet-world.greet)")
    ]

timelineRepl :: [(Int, Actor, StateT Int (ReaderT Env IO) ())]
timelineRepl =
  [ (seconds 3, Actor_Broadcast, broadcastPactTransaction 0 "(+ 1 2)")
  , (seconds 2, Actor_Broadcast, broadcastPactTransaction 1 "(+ 2 3)")
  ]

runActor :: Env -> Actor -> IO ()
runActor env actor = flip runReaderT env $ flip evalStateT 0 $
  for_ timelineEntries $ \(entryDelay, entryActor, entryAction) ->
    when (actor == entryActor) $ do
      liftIO $ threadDelay entryDelay
      entryAction

{- Tendermint RPC -}
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

{- Tendermint CLI -}
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
  , _nodeFlags_emptyBlocks :: Bool
  }

mkNetworkFlags :: NetworkFlags
mkNetworkFlags = NetworkFlags
  { _networkFlags_validators    = networkSize
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
  , _nodeFlags_emptyBlocks = False
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
  , "--consensus.create_empty_blocks" <> _UPSTREAM_ "incoherent with other args" "=" <> tshow (_nodeFlags_emptyBlocks nf)
  ]

tendermintNodeId :: GlobalFlags -> Sh Text
tendermintNodeId gf = tendermint gf "show_node_id" []

{- Network addresses -}
defaultTendermintP2PHostPort :: (Text, Int)
defaultTendermintP2PHostPort = ("127.0.0.1", 26656)

defaultTendermintRPCHostPort :: (Text, Int)
defaultTendermintRPCHostPort = ("127.0.0.1", 26657)

defaultABCIAppHostPort :: IsString a => (a, Int)
defaultABCIAppHostPort = ("127.0.0.1", 26658)

{- ABCI app -}
runABCI :: Int -> IO ()
runABCI nid = do
  rs <- Pact.initReplState Pact.StringEval Nothing
  _logger <- mkLogStdout -- too noisy

  let
    env = abciEnv nid

    transformHandler :: HandlerT (Response t) -> IO (Response t)
    transformHandler er = do
      x <- runExceptT $ runReaderT er env
      case x of
        Right r -> pure r
        Left l -> pure $ ResponseException $ def
          & _exceptionError .~ l

  serveAppWith (mkServerSettings $ mkABCIHostPort nid) mempty
    $ transformApp transformHandler
    $ app nid rs

type Err = Text
type HandlerT = ReaderT Env (ExceptT Err IO)
type HandlerEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

app :: HandlerEffects m => Int -> Pact.ReplState -> App m
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

check :: HandlerEffects m => Int -> Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check nid = runPactTransaction nid accept reject True
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

deliver :: HandlerEffects m => Int -> Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver nid = runPactTransaction nid accept reject False
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1

runPactTransaction :: HandlerEffects m => nid -> m a -> m a -> Bool -> Pact.ReplState -> HexString -> m a
runPactTransaction _nid accept reject shouldRollback rs hx = do
  libState <- liftIO $ readMVar $ rs ^. Pact.rEnv ^. Pact.eePactDbVar
  let dbEnvVar = libState ^. Pact.rlsPure
  oldDb <- liftIO $ view Pact.db <$> readMVar dbEnvVar

  res <- runPactCode _nid accept reject shouldRollback rs hx

  when shouldRollback $ liftIO $ do
    modifyMVar_ dbEnvVar $ pure . (Pact.db .~ oldDb)

  pure res

runPactCode :: HandlerEffects m => nid -> m a -> m a -> Bool -> Pact.ReplState -> HexString -> m a
runPactCode _nid accept reject shouldRollback rs hx = rejectOnError $ do
  txt <- decode hx
  pt <- parse txt

  log (bool "Delivering" "Checking" shouldRollback <> " transaction " <> tshow (_pactTransaction_nonce pt)) Nothing

  snapshot <- snapshotPactState

  r <- eval $ _pactTransaction_code pt
  log "Pact result" (Just $ T.strip $ tshow r)

  when shouldRollback $ restorePactState snapshot

  where
    decode = withExceptT (\err -> ("Failed decode with error", Just $ tshow err))
      . liftEither . decodeHexString

    parse txt = liftEither $ case T.readMaybe (T.unpack txt) of
      Nothing -> Left ("Failed to parse transaction", Nothing)
      Just p -> Right p

    eval code = withExceptT (\err -> ("Pact error", Just $ T.pack err))
      $ ExceptT $ liftIO $ (evalStateT (Pact.evalRepl' $ T.unpack code) rs)

    snapshotPactState = liftIO $ do
      libState <- readMVar $ rs ^. Pact.rEnv ^. Pact.eePactDbVar
      let dbEnvVar = libState ^. Pact.rlsPure
      oldDb <- view Pact.db <$> readMVar dbEnvVar
      pure (dbEnvVar, oldDb)

    restorePactState (dbEnvVar, oldDb) = liftIO $
      modifyMVar_ dbEnvVar $ pure . (Pact.db .~ oldDb)

    rejectOnError = runExceptT >=> \case
      Left (h,b) -> log h b *> reject
      Right () -> accept

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

{- Pact code samples -}
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
