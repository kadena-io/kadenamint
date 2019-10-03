{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Kadenamint where

import Control.Arrow                                    (left)
import Control.Concurrent                               (threadDelay)
import Control.Concurrent.Async                         (async, cancel, forConcurrently_, withAsync)
import Control.Concurrent.MVar                          (modifyMVar_, readMVar)
import Control.Lens                                     (imap, strict, view, (&), (^.), (.~))
import Control.Monad                                    ((>=>))
import Control.Monad.Except                             (MonadError(..), ExceptT(..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), asks, runReaderT)
import Control.Monad.State.Strict                       (StateT(..), evalStateT)
import Control.Monad.Trans.Class                        (lift)
import Data.Binary.Builder                              (toLazyByteString)
import Data.Bool                                        (bool)
import Data.ByteArray.Encoding                          (Base(Base16), convertToBase)
import Data.Colour.SRGB                                 (Colour, sRGB24)
import Data.Conduit.Network                             (serverSettings)
import Data.Default                                     (Default(..))
import Data.FileEmbed                                   (embedStringFile)
import Data.Functor                                     (void)
import Data.Maybe                                       (fromMaybe)
import Data.String                                      (IsString(..))
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import qualified Data.Text.IO as T
import Data.Time                                        (UTCTime, getCurrentTime)
import Data.Traversable                                 (for)
import GHC.Generics                                     (Generic)
import Network.HTTP.Types                               (encodePath)
import Shelly                                           (Sh, cp, shelly, silently, run, run_, toTextIgnore, withTmpDir, (</>))
import qualified Shelly as Sh
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..), setSGRCode)
import System.Which                                     (staticWhich)
import Text.Read as T                                   (readMaybe)
import qualified Toml

import Prelude                                          hiding (head, log)

import Data.ByteArray.HexString                         (HexString(..))
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger     (mkLogStdout)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..), InitChain(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.PersistPactDb as Pact
import qualified Pact.Repl as Pact
import qualified Pact.Repl.Types as Pact
import qualified Pact.Types.Pretty as Pact
import qualified Pact.Types.Runtime as Pact

import Kadenamint.Tendermint

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

coreEnv :: Maybe Text -> Env
coreEnv moniker = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground red] $ mconcat
      [ "\n[CORE] "
      , maybe "" (\m -> "Node: " <> tshow m <> " | ") moniker
      , x
      ]
  }

abciEnv :: Text -> Env
abciEnv moniker = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground green] $ "\n[ABCI] Node: " <> moniker <> " | " <> x
  }

data NodePorts = NodePorts
  { _nodePorts_p2p :: Int
  , _nodePorts_rpc :: Int
  , _nodePorts_abci :: Int
  } deriving (Eq, Ord, Read, Show, Generic)

nodePortsOffset :: Int
nodePortsOffset = 10

nthNodePorts :: Int -> NodePorts
nthNodePorts index =
  let offset = 26656 + nodePortsOffset * index
  in NodePorts offset (offset + 1) (offset + 2)

extraNodePorts :: NodePorts
extraNodePorts = nthNodePorts (negate 1)

addNode :: MonadIO m => Text -> Text -> NodePorts -> InitializedNode -> m InitializedNode
addNode home moniker ports preExistingNode = shelly $ do
  void $ tendermint (GlobalFlags home) "init" []
  n <- loadNode home

  cp (genesisFile preExistingNode) (configDir n)
  let
    newCfg = _initializedNode_config preExistingNode
      & config_moniker .~ moniker
      & updatePorts ports
  storeConfig home newCfg

  pure $ n &
    initializedNode_config .~ newCfg

runNode :: MonadIO m => InitializedNode -> m ()
runNode n = void $ do
  let
    cfg = _initializedNode_config n
    moniker = _config_moniker cfg

  liftIO $ withAsync (runABCI cfg) $ \_ ->
    flip runReaderT (coreEnv $ Just moniker) $ do
      log "Launching" Nothing
      shelly $ tendermintNode $ GlobalFlags $ _initializedNode_home n

initNetwork :: MonadIO m => Text -> Int -> m [InitializedNode]
initNetwork root size = shelly $ do
  void $ tendermintNetwork $ mkNetworkFlags root size
  for [0..size-1] $ \i -> do
    let
      home = root <> "/node" <> tshow i
    n <- loadNode home
    let
      oldCfg = n ^. initializedNode_config
      ports = nthNodePorts i

      peers = T.splitOn "," $ oldCfg ^. config_p2p . configP2P_persistentPeers
      peers' = T.intercalate "," $ flip imap peers $ \j peer ->
        let (nid, rest) = cleave "@" peer
            (_host, port) = cleave ":" rest
            port' = case T.readMaybe (T.unpack port) of
              Nothing -> error "parsing error"
              Just p -> p + j * nodePortsOffset
        in nid <> "@" <> localhost <> ":" <> tshow port'

    let cfg = oldCfg
          & config_moniker .~ "node" <> tshow i
          & updatePorts ports
          & config_p2p . configP2P_persistentPeers .~ peers'
          & config_p2p . configP2P_privatePeerIds .~ peers'
          & config_p2p . configP2P_addrBookStrict .~ False
          & config_p2p . configP2P_allowDuplicateIp .~ True
          & config_consensus . configConsensus_createEmptyBlocksInterval .~ "5s"

    storeConfig home cfg
    pure $ n & initializedNode_config .~ cfg

updatePorts :: NodePorts -> Config -> Config
updatePorts (NodePorts p2p rpc abci) cfg = cfg
  & config_p2p . configP2P_laddr .~ l p2p
  & config_rpc . configRPC_laddr .~ l rpc
  & config_proxyApp .~ l abci
  where
    l p = "tcp://" <> localhost <> ":" <> tshow p

localhost :: Text
localhost = "127.0.0.1"

runEverything :: IO ()
runEverything = timelineCoinContract

withNetwork
  :: Int
  -> (Text -> [InitializedNode] -> StateT Int IO ())
  -> IO ()
withNetwork size f = shelly $ withTmpDir $ \(toTextIgnore -> root) -> do
  genesisNodes <- initNetwork root size

  flip runReaderT (coreEnv Nothing) $
    log ("Network of size " <> tshow size <> " has been setup at " <> root) Nothing

  liftIO $ withAsync (runTimeline $ f root genesisNodes) $ \_ ->
    forConcurrently_ genesisNodes runNode

type Timeline m = (MonadReader Env m, MonadIO m)

showBalances :: Text
showBalances = [here|
  { "k1" : (coin.account-balance 'k1), "k2" : (coin.account-balance 'k2), "k3" : (coin.account-balance 'k3)}
|]

debit :: Text -> Double -> Text
debit from amount = T.intercalate " "
  [ "(coin.debit"
  , from
  , tshow amount
  , ")"
  ]

credit :: Text -> Double -> Text
credit to amount = T.intercalate " "
  [ "(coin.credit"
  , to
  , "(read-keyset " <> to <> ")"
  , tshow amount
  , ")"
  ]

transfer :: Text -> Text -> Double -> Text
transfer from to amount = T.unlines [debit from amount, credit to amount]

coinReplEnv :: Text
coinReplEnv = [here|
  (env-data { "k1" : ["keys1"], "k2": ["keys2"], "k3": ["keys3"] })
  (env-keys ["keys1", "keys2", "keys3", "keys4"])
  (test-capability (coin.TRANSFER))
|]

showBalancesTx :: MonadIO m => InitializedNode -> m ()
showBalancesTx = broadcastPactText (coinReplEnv <> showBalances)

debitTx :: MonadIO m => Text -> Double -> InitializedNode -> m ()
debitTx from amount = broadcastPactText (coinReplEnv <> debit from amount <> showBalances)

creditTx :: MonadIO m => Text -> Double -> InitializedNode -> m ()
creditTx to amount = broadcastPactText (coinReplEnv <> credit to amount <> showBalances)

transferTx :: MonadIO m => Text -> Text -> Double -> InitializedNode -> m ()
transferTx from to amount = broadcastPactText (coinReplEnv <> transfer from to amount <> showBalances)

timelineCoinContract :: IO ()
timelineCoinContract = withNetwork 2 $ \root -> \case
  [n0, n1] -> do
    sleep 2
    showBalancesTx n1

    sleep 2
    n3 <- addNode (root <> "/nodeX") "nodeX" extraNodePorts n0
    a3 <- liftIO $ async $ runNode n3

    sleep 2
    showBalancesTx n3

    sleep 2
    transferTx "'k3" "'k1" 0.5 n3

    sleep 2
    liftIO $ cancel a3
    flip runReaderT (coreEnv Nothing) $ log "Stopping nodeX" Nothing

    sleep 2
    transferTx "'k3" "'k2" 0.5 n0

    sleep 2
    void $ liftIO $ async $ runNode n3


  _ -> impossible

timelineHelloWorld :: IO ()
timelineHelloWorld = withNetwork 3 $ \root -> \case
  [n0, n1, _n2] -> do
    sleep 3 *> broadcastPactFile "pact/hello-world.pact" n0
    sleep 2 *> broadcastPactText "(hello-world.set-message \"hello\")" n1

    sleep 1
    n3 <- addNode (root <> "/nodeX") "nodeX" extraNodePorts n0
    void $ liftIO $ async $ runNode n3

    sleep 3 *> broadcastPactText "(hello-world.greet)" n3
  _ -> impossible

timelineRepl :: IO ()
timelineRepl = withNetwork 2 $ \_ -> \case
  [n0, n1] -> do
    sleep 3 *> broadcastPactText "(+ 1 2)" n0
    sleep 2 *> broadcastPactText "(+ 1 2)" n1
  _ -> impossible

runTimeline :: StateT Int IO a -> IO a
runTimeline = flip evalStateT 0

{- Tendermint RPC -}
type Nonce = Int

data PactTransaction = PactTransaction
  { _pactTransaction_nonce :: UTCTime
  , _pactTransaction_code  :: PactCode
  } deriving (Eq, Ord, Read, Show, Generic)

data PactCode
  = PactCode_Text Text
  | PactCode_File Text
  deriving (Eq, Ord, Read, Show, Generic)

broadcastPactText :: MonadIO m => Text -> InitializedNode -> m ()
broadcastPactText txt n = broadcastPact (PactCode_Text txt) n

broadcastPactFile :: MonadIO m => Text -> InitializedNode -> m ()
broadcastPactFile path n = do
  p <- shelly $ Sh.absPath $ Sh.fromText $ _ASSUME_ "network running on local host " path
  broadcastPact (PactCode_File $ Sh.toTextIgnore p) n

broadcastPact :: MonadIO m => PactCode -> InitializedNode -> m ()
broadcastPact code n = do
  let
    cfg = _initializedNode_config n
    rpc' = _configRPC_laddr $ _config_rpc cfg
    rpc = fromMaybe rpc $ T.stripPrefix "tcp://" rpc'

  nonce <- liftIO $ getCurrentTime

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact code to node #" <> _config_moniker cfg <> " at " <> rpc) (Just $ tshow code)
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
loadNode :: MonadIO m => Text -> m InitializedNode
loadNode home = pure (InitializedNode home)
  <*> loadConfig home
  <*> shelly (silently $ fmap T.strip $ tendermint (GlobalFlags home) "show_node_id" [])

loadConfig :: MonadIO m => Text -> m Config
loadConfig home = liftIO $ do
  toml <- T.readFile $ T.unpack $ home <> "/config" <> "/config.toml"
  case Toml.decode configCodec toml of
    Left err -> do
      print toml
      print $ err
      error "Failed decoding"
    Right config -> pure config


storeConfig :: MonadIO m => Text -> Config -> m ()
storeConfig home = liftIO . T.writeFile (T.unpack $ home <> "/config" <> "/config.toml") . Toml.encode configCodec

genesisFile :: InitializedNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: InitializedNode -> Sh.FilePath
configDir n = _initializedNode_home n </> ("config" :: Text)

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = runCmd $ tendermintCmd gf tmCmd cmdArgs

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork = runCmd . tendermintNetworkCmd

tendermintNode :: GlobalFlags -> Sh Text
tendermintNode = runCmd . tendermintNodeCmd

runCmd :: (Sh.FilePath, [Text]) -> Sh Text
runCmd = uncurry run

plainCmd :: (Sh.FilePath, [Text]) -> Text
plainCmd (cmd, args) = T.intercalate " " $ Sh.toTextIgnore cmd : fmap singleQuotes args

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermintCmd :: GlobalFlags -> Text -> [Text] -> (Sh.FilePath, [Text])
tendermintCmd gf tmCmd cmdArgs = (tendermintPath,) $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetworkCmd :: NetworkFlags -> (Sh.FilePath, [Text])
tendermintNetworkCmd nf = (tendermintPath,) $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  , "--starting-ip-address", localhost
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

tendermintNodeCmd :: GlobalFlags -> (Sh.FilePath, [Text])
tendermintNodeCmd gf = tendermintCmd gf "node" []

{- ABCI app -}
runABCI :: Config -> IO ()
runABCI cfg = do
  rs <- Pact.initReplState Pact.StringEval Nothing
  _logger <- mkLogStdout -- too noisy

  let
    env = abciEnv $ _config_moniker cfg

    (_protocol, rest) = cleave "://" (cfg ^. config_proxyApp)
    (host, port) = cleave ":" rest
    port' = case T.readMaybe (T.unpack port) of
      Nothing -> error "parsing error"
      Just p -> p
    host' = fromString $ T.unpack host

    transformHandler :: HandlerT (Response t) -> IO (Response t)
    transformHandler er = do
      x <- runExceptT $ runReaderT er env
      case x of
        Right r -> pure r
        Left l -> pure $ ResponseException $ def
          & _exceptionError .~ l

  serveAppWith (serverSettings port' host') mempty
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
  RequestInitChain ic -> initChain rs ic
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx (CheckTx hx) -> check rs hx
  RequestDeliverTx (DeliverTx hx) -> deliver rs hx
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

coinPactFile :: Text
coinPactFile = $(embedStringFile "pact/coin-contract/coin.pact")

coinReplFile :: Text
coinReplFile = $(embedStringFile "pact/coin-contract/coin.repl")

initChain :: HandlerEffects m => Pact.ReplState -> InitChain -> m (Response 'MTInitChain)
initChain rs _ic = abortOnError $ do
  void $ ExceptT $ runPactCode rs coinPactFile
  log "Initialized coin contract" Nothing
  void $ ExceptT $ runPactCode rs coinReplFile
  log "Initialized coin accounts" Nothing

  where
    abortOnError = runExceptT >=> \case
      Right _termName -> pure def
      Left err -> do
        log "Init chain failed" (Just err)
        error $ T.unpack err

check :: HandlerEffects m => Pact.ReplState -> HexString -> m (Response 'MTCheckTx)
check rs hx = withPactRollback rs $ runPactTransaction logParsed logEvaluated accept reject rs hx
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1
    logParsed pt = log ("Checking transaction with nonce: " <> tshow (_pactTransaction_nonce pt)) Nothing
    logEvaluated _ = pure ()


deliver :: HandlerEffects m => Pact.ReplState -> HexString -> m (Response 'MTDeliverTx)
deliver = runPactTransaction logParsed logEvaluated accept reject
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1
    logParsed pt = log ("Delivering transaction with nonce: " <> tshow (_pactTransaction_nonce pt)) Nothing
    logEvaluated r = log "Pact result" (Just $ T.strip $ tshow $ Pact.pretty r)


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

runPactCode :: HandlerEffects m => Pact.ReplState -> Text -> m (Either Text (Pact.Term Pact.Name))
runPactCode rs code = liftIO $ fmap (left T.pack) $ evalStateT (Pact.evalRepl' $ T.unpack code) rs

runPactTransaction :: HandlerEffects m => (PactTransaction -> m ()) -> (Pact.Term Pact.Name -> m ()) -> m a -> m a -> Pact.ReplState -> HexString -> m a
runPactTransaction logParsed logEvaluated accept reject rs hx = rejectOnError $ do
  txt <- decode hx
  pt <- parse txt

  lift $ logParsed pt

  r <- eval =<< case _pactTransaction_code pt of
    PactCode_Text t -> pure t
    PactCode_File f -> shelly $ Sh.readfile $ Sh.fromText f

  lift $ logEvaluated r

  where
    decode = withExceptT (\err -> ("Failed decode with error", Just $ tshow err))
      . liftEither . decodeHexString

    eval = withExceptT (\err -> ("Pact error", Just err)) . ExceptT . runPactCode rs

    parse txt = liftEither $ case T.readMaybe (T.unpack txt) of
      Nothing -> Left ("Failed to parse transaction", Nothing)
      Just p -> Right p

    rejectOnError = runExceptT >=> \case
      Left (h,b) -> log ("Rejecting transaction - " <> h) b *> reject
      Right () -> accept

{- Utils -}
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

cleave :: Text -> Text -> (Text, Text)
cleave sep str =
  let (a,b) = T.breakOn sep str
  in (a, T.drop (T.length sep) b)

singleQuotes :: (IsString a, Semigroup a) => a -> a
singleQuotes t = "'" <> t <> "'"

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

_ASSUME_ :: Text -> a -> a
_ASSUME_ _ = id
