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
import Control.Concurrent.Async                         (async, forConcurrently_, withAsync)
import Control.Concurrent.MVar                          (modifyMVar_, readMVar)
import Control.Lens                                     (strict, view, _2, (&), (^.), (.~), (+~))
import Control.Monad                                    ((>=>))
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
import Shelly                                           (Sh, cp, shelly, silently, run, run_, (</>))
import qualified Shelly as Sh
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..), setSGRCode)
import System.Which                                     (staticWhich)
import Text.Read as T                                   (readMaybe)

import Prelude                                          hiding (head, log)

import Data.ByteArray.HexString                         (HexString(..))
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger     (mkLogStdout)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.PersistPactDb as Pact
import qualified Pact.Repl as Pact
import qualified Pact.Repl.Types as Pact
import qualified Pact.Types.Pretty as Pact
import qualified Pact.Types.Runtime as Pact

{- Process orchestration -}
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

initNode :: MonadIO m => Maybe InitializedNode -> Int -> m InitializedNode
initNode preExistingNode i = shelly $ do
  void $ tendermint (mkGlobalFlags i) "init" []
  let n = mkInitializedNode i
  for_ preExistingNode $ \en ->
    cp (genesisFile en) (configDir n)
  pure n

initStandaloneNode :: MonadIO m => Int -> m InitializedNode
initStandaloneNode = initNode Nothing

initExtraNode :: MonadIO m => InitializedNode -> Int -> m InitializedNode
initExtraNode = initNode . Just

launchNode :: MonadIO m => [(Int, Text)] -> InitializedNode -> m ()
launchNode persistentPeers n = void $ do
  let i = _initializedNode_index n
  liftIO $ withAsync (runABCI i) $ \_ ->
    flip runReaderT (coreEnv $ Just i) $ do
      liftIO $ threadDelay $ seconds i `div` 10
      log "Launching" Nothing
      shelly $ tendermintNode (mkGlobalFlags i) (mkNodeFlags persistentPeers i)

initNetwork :: MonadIO m => Int -> m [InitializedNode]
initNetwork size = do
  void $ shelly $ tendermintNetwork $ mkNetworkFlags size
  pure $ fmap mkInitializedNode $ [0..size-1]

runEverything :: IO ()
runEverything = timelineCoinContract

withNetwork
  :: Int
  -> ([(Int, Text)] -> [InitializedNode] -> StateT Int (ReaderT Env IO) ())
  -> IO ()
withNetwork size f = do
  let
    deleteNetwork = run_ "rm" ["-rf", _networkFlags_output $ mkNetworkFlags size]

    resetNetwork = do
      shelly deleteNetwork
      res <- initNetwork size
      log "Network has been reset" Nothing
      pure res

  genesisNodes <- flip runReaderT (coreEnv Nothing) resetNetwork

  peers <- for genesisNodes $ \gn -> do
    ni <- shelly $ silently $ tendermintNodeId gn
    pure (_initializedNode_index gn, T.strip ni)

  withAsync (runTimeline broadcastEnv $ f peers genesisNodes) $ \_ ->
    forConcurrently_ genesisNodes (launchNode peers)

type Timeline m = (MonadState Int m, MonadReader Env m, MonadIO m)

timelineCoinContract :: IO ()
timelineCoinContract = withNetwork 3 $ \peers -> \case
  [n0, n1, n2] -> do
    sleep 3
    broadcastPactFile n0 "pact/coin-contract/coin.pact"

    sleep 1
    n3 <- initExtraNode n0 3
    liftIO $ void $ async $ launchNode peers n3

    sleep 4
    broadcastPactFile n1 "pact/coin-contract/coin.repl"

    sleep 3
    broadcastPactText n2
      [here|
           (use coin)
           { "k1" : (account-balance 'k1), "k2" : (account-balance 'k2), "k3" : (account-balance 'k3)}
      |]

    sleep 3
    broadcastPactText n3
      [here|
           (use coin)

           (env-data { "k1" : ["keys1"], "k2": ["keys2"], "k3": ["keys3"] })
           (env-keys ["keys1", "keys2", "keys3", "keys4"])
           (define-keyset 'k1 (read-keyset "k1"))
           (define-keyset 'k2 (read-keyset "k2"))
           (define-keyset 'k3 (read-keyset "k3"))
           (test-capability (TRANSFER))

           (debit 'k3 0.5)
           (credit 'k1 (read-keyset 'k1) 0.5)
      |]

    sleep 3
    broadcastPactText n3
      [here|
           (use coin)
           { "k1" : (account-balance 'k1), "k2" : (account-balance 'k2), "k3" : (account-balance 'k3)}
      |]
  _ -> impossible

timelineHelloWorld :: IO ()
timelineHelloWorld = withNetwork 3 $ \peers -> \case
  [n0, n1, _n2] -> do
    sleep 3 *> broadcastPactFile n0 "pact/hello-world.pact"
    sleep 2 *> broadcastPactText n1 "(hello-world.set-message \"hello\")"

    sleep 1
    n3 <- initExtraNode n0 3
    liftIO $ void $ async $ launchNode peers n3

    sleep 3 *> broadcastPactText n3 "(hello-world.greet)"
  _ -> impossible

timelineRepl :: IO ()
timelineRepl = withNetwork 2 $ \_ -> \case
  [n0, n1] -> do
    sleep 3 *> broadcastPactText n0 "(+ 1 2)"
    sleep 2 *> broadcastPactText n1 "(+ 2 3)"
  _ -> impossible

runTimeline :: Env -> StateT Int (ReaderT Env IO) a -> IO a
runTimeline env = flip runReaderT env . flip evalStateT 0

{- Tendermint RPC -}
data PactTransaction = PactTransaction
  { _pactTransaction_nonce :: Int
  , _pactTransaction_code  :: PactCode
  } deriving (Eq, Ord, Read, Show)

data PactCode
  = PactCode_Text Text
  | PactCode_File Text
  deriving (Eq, Ord, Read, Show)

broadcastPactText :: Timeline m => InitializedNode -> Text -> m ()
broadcastPactText n = broadcastPact n . PactCode_Text

broadcastPactFile :: Timeline m => InitializedNode -> Text -> m ()
broadcastPactFile n path = do
  p <- shelly $ Sh.absPath $ Sh.fromText path
  broadcastPact n $ PactCode_File $ Sh.toTextIgnore p

broadcastPact :: Timeline m => InitializedNode -> PactCode -> m ()
broadcastPact n code = do
  let
    i = _initializedNode_index n
    rpc = mkRPCAddress i
  log ("Broadcasting pact code to node #" <> tshow i <> " at " <> rpc) (Just $ tshow code)
  nonce <- get
  modify (+1)
  broadcastTransaction rpc $ tshow $ PactTransaction nonce code

broadcastTransaction :: (MonadIO m, MonadReader Env m) => Text -> Text -> m ()
broadcastTransaction addr t = do
  let txHex = "0x" <> convertToBase Base16 (T.encodeUtf8 t)
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath [toEndpoint BroadcastTx_Commit] [("tx", Just txHex)] of
    Left err -> do
      log "Failed encoding of transaction with error" (Just $ tshow err)
      fatal
    Right pathAndQuery -> do
      let url = addr <> pathAndQuery
      log "Broadcasting at" (Just url)
      shelly $ silently $ run_ "curl" [url] & _TODO_ "handle timeout and errors"

data BroadcastTx
  = BroadcastTx_Async
  | BroadcastTx_Sync
  | BroadcastTx_Commit

toEndpoint :: BroadcastTx -> Text
toEndpoint = \case
  BroadcastTx_Async -> f "async"
  BroadcastTx_Sync -> f "sync"
  BroadcastTx_Commit -> f "commit"
  where
    f = ("broadcast_tx_" <>)

{- Tendermint CLI -}
data InitializedNode = InitializedNode
  { _initializedNode_index :: Int
  , _initializedNode_home :: Text
  }

type Address = Text
type Peer = (Text, Address)

newtype GlobalFlags = GlobalFlags
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
  , _nodeFlags_privatePeers :: [Peer]
  , _nodeFlags_emptyBlocks :: Bool
  }

mkNetworkHome :: Text
mkNetworkHome = "./.tendermint"
mkNetworkFlags :: Int -> NetworkFlags
mkNetworkFlags size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = mkNetworkHome
  , _networkFlags_populatePeers = True
  }

mkInitializedNode :: Int -> InitializedNode
mkInitializedNode = InitializedNode <*> mkNodeHomePath

mkNodeHomePath :: Int -> Text
mkNodeHomePath i = mkNetworkHome <> "/node" <> tshow i

mkGlobalFlags :: Int -> GlobalFlags
mkGlobalFlags i = GlobalFlags { _globalFlags_home = mkNodeHomePath i }

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
  , _nodeFlags_peers = ps
  , _nodeFlags_privatePeers = ps
  , _nodeFlags_emptyBlocks = False
  }
  where
    ps = flip fmap peers $ \(i', pid) -> (pid, mkP2PAddress i')

genesisFile :: InitializedNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: InitializedNode -> Sh.FilePath
configDir n = _initializedNode_home n </> ("config" :: Text)

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
  , "--p2p.private_peer_ids", T.intercalate "," $ fmap fst (_nodeFlags_privatePeers nf)
    & _TODO_ (mconcat
               [ "figure out if there's a better way to hush logs:"
               , "see https://github.com/tendermint/tendermint/issues/1215"
               , "& https://github.com/tendermint/tendermint/pull/3474"
               ])
  , "--consensus.create_empty_blocks" <> _UPSTREAM_ "incoherent with other args" "=" <> tshow (_nodeFlags_emptyBlocks nf)
  ]

tendermintNodeId :: InitializedNode -> Sh Text
tendermintNodeId n = tendermint (mkGlobalFlags $ _initializedNode_index n) "show_node_id" []

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
    $ app rs

type Err = Text
type HandlerT = ReaderT Env (ExceptT Err IO)
type HandlerEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

app :: HandlerEffects m => Pact.ReplState -> App m
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

check :: HandlerEffects m => Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check rs hx = withPactRollback rs $ runPactTransaction "Checking" accept reject rs hx
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

deliver :: HandlerEffects m => Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver = runPactTransaction "Delivering" accept reject
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1

withPactRollback :: HandlerEffects m => Pact.ReplState -> m a -> m a
withPactRollback rs action = do
  snapshot <- snapshotPactState
  res <- action
  restorePactState snapshot
  pure res

  where
    snapshotPactState = liftIO $ do
      libState <- readMVar $ rs ^. Pact.rEnv . Pact.eePactDbVar
      let dbEnvVar = libState ^. Pact.rlsPure
      oldDb <- view Pact.db <$> readMVar dbEnvVar
      pure (dbEnvVar, oldDb)

    restorePactState (dbEnvVar, oldDb) = liftIO $
      modifyMVar_ dbEnvVar $ pure . (Pact.db .~ oldDb)

runPactTransaction :: HandlerEffects m => Text -> m a -> m a -> Pact.ReplState -> HexString -> m a
runPactTransaction hook accept reject rs hx = rejectOnError $ do
  txt <- decode hx
  pt <- parse txt

  log (hook <> " transaction " <> tshow (_pactTransaction_nonce pt)) Nothing

  r <- eval =<< case _pactTransaction_code pt of
    PactCode_Text t -> pure t
    PactCode_File f -> shelly $ Sh.readfile $ Sh.fromText f

  log "Pact result" (Just $ T.strip $ tshow $ Pact.pretty r)

  where
    decode = withExceptT (\err -> ("Failed decode with error", Just $ tshow err))
      . liftEither . decodeHexString

    parse txt = liftEither $ case T.readMaybe (T.unpack txt) of
      Nothing -> Left ("Failed to parse transaction", Nothing)
      Just p -> Right p

    eval code = withExceptT (\err -> ("Pact error", Just $ T.pack err))
      $ ExceptT $ liftIO $ evalStateT (Pact.evalRepl' $ T.unpack code) rs

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
  liftIO $ putStrLn $ T.unpack $ p $ header <> maybe "" (\b -> ":\n" <> T.unlines (("  " <>) <$> T.lines b)) body

sgrify :: [SGR] -> Text -> Text
sgrify codes txt = mconcat
  [ T.pack $ setSGRCode codes
  , txt
  , T.pack $ setSGRCode [Reset]
  ]

seconds :: Int -> Int
seconds = (*1e6)

fatal :: a
fatal = error "fatal error"

impossible :: a
impossible = error "the 'impossible' has happened"

doubleQuotes :: (IsString a, Semigroup a) => a -> a
doubleQuotes t = "\"" <> t <> "\""

decodeHexString :: HexString -> Either T.UnicodeException Text
decodeHexString (HexString bs) = T.decodeUtf8' bs & _TODO_ "make sure this is the right decoding"

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . threadDelay . seconds

{- Issue tracking -}
_UPSTREAM_ :: Text -> a -> a
_UPSTREAM_ _ = id

_TODO_ :: Text -> a -> a
_TODO_ _ = id
