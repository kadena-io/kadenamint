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

import Control.Concurrent                               (threadDelay)
import Control.Concurrent.Async                         (async, cancel, forConcurrently_, withAsync)
import Control.Lens                                     (imap, strict, view, (&), (^.), (.~))
import Control.Monad                                    ((>=>))
import Control.Monad.Except                             (MonadError(..), ExceptT(..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), asks, runReaderT)
import Control.Monad.State.Strict                       (StateT(..), evalStateT)
import Control.Monad.Trans.Class                        (lift)
import qualified Data.Aeson as Aeson
import Data.Binary.Builder                              (toLazyByteString)
import Data.Bool                                        (bool)
import Data.ByteArray.Encoding                          (Base(Base16), convertToBase)
import Data.Colour.SRGB                                 (Colour, sRGB24)
import Data.Conduit.Network                             (serverSettings)
import Data.Default                                     (Default(..))
import Data.Functor                                     (void)
import Data.Maybe                                       (fromMaybe)
import Data.String                                      (IsString(..))
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import qualified Data.Text.IO as T
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
import qualified Pact.Interpreter as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Pretty as Pact

import Kadenamint.Pact
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

runNodeDir :: MonadIO m => Text -> m ()
runNodeDir dir = loadNode dir >>= runNode

runNode :: MonadIO m => InitializedNode -> m ()
runNode n = void $ do
  liftIO $ withAsync (runABCI n) $ \_ ->
    flip runReaderT (coreEnv $ Just $ _config_moniker $ _initializedNode_config n) $ do
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
  { "sender00" : (coin.account-balance 'sender00)
  , "sender01" : (coin.account-balance 'sender01)
  , "sender02" : (coin.account-balance 'sender02)
  }
|]

transfer :: Text -> Text -> Double -> Text
transfer from to amount = T.intercalate " "
  [ "(coin.transfer"
  , "'" <> from
  , "'" <> to
  , tshow amount
  , ")"
  ]

showBalancesTx :: MonadIO m => InitializedNode -> m ()
showBalancesTx = broadcastPact showBalances

showBalanceTx :: MonadIO m => Text -> InitializedNode -> m ()
showBalanceTx acct = broadcastPact ("(coin.account-balance '" <> acct <> ")")

transferTx :: MonadIO m => Text -> Text -> Double -> InitializedNode -> m ()
transferTx from to amount = broadcastPactSigned (Just from) (transfer from to amount <> showBalances)

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
    transferTx "sender00" "sender01" 1 n3

    sleep 2
    liftIO $ cancel a3
    flip runReaderT (coreEnv Nothing) $ log "Stopping nodeX" Nothing

    sleep 2
    transferTx "sender00" "sender02" 1 n0

    sleep 2
    void $ liftIO $ async $ runNode n3


  _ -> impossible

timelineRepl :: IO ()
timelineRepl = withNetwork 2 $ \_ -> \case
  [n0, n1] -> do
    sleep 3 *> broadcastPact "(+ 1 2)" n0
    sleep 2 *> broadcastPact "(+ 1 2)" n1
  _ -> impossible


runTimeline :: StateT Int IO a -> IO a
runTimeline = flip evalStateT 0

{- Tendermint RPC -}
type Nonce = Int

broadcastPact :: MonadIO m => Text -> InitializedNode -> m ()
broadcastPact = broadcastPactSigned Nothing

broadcastPactSigned :: MonadIO m => Maybe Text -> Text -> InitializedNode -> m ()
broadcastPactSigned sender code n = do
  let
    cfg = _initializedNode_config n
    rpc' = _configRPC_laddr $ _config_rpc cfg
    rpc = fromMaybe rpc $ T.stripPrefix "tcp://" rpc'

  cmd <- mkExec' code sender

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact code to node #" <> _config_moniker cfg <> " at " <> rpc) (Just $ tshow code)
    broadcastTransaction rpc $ tshow $ Aeson.toJSON cmd

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
runABCI :: InitializedNode -> IO ()
runABCI n = do
  let cfg = n ^. initializedNode_config
      home = n ^. initializedNode_home

  pactDbEnv <- initDb $ T.unpack home <> "/pact-db"

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
    $ app pactDbEnv

type Err = Text
type HandlerT = ReaderT Env (ExceptT Err IO)
type HandlerEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

app :: HandlerEffects m => DB -> App m
app pactDbEnv = App $ \case
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain ic -> initChain pactDbEnv ic
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx (CheckTx hx) -> check pactDbEnv hx
  RequestDeliverTx (DeliverTx hx) -> deliver pactDbEnv hx
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

initChain :: HandlerEffects m => DB -> InitChain -> m (Response 'MTInitChain)
initChain pactDbEnv _ic = abortOnError $ do
  let eval = execYaml pactDbEnv
  void $ eval "pact/coin-contract/load-coin-contract.yaml"
  log "Initialized coin contract" Nothing
  void $ eval "pact/coin-contract/grants.yaml"
  log "Initialized coin accounts" Nothing

  where
    abortOnError = runExceptT >=> \case
      Right _termName -> pure def
      Left err -> do
        log "Init chain failed" (Just err)
        error $ T.unpack err

check :: HandlerEffects m => DB -> HexString -> m (Response 'MTCheckTx)
check pactEnv hx = runPactTransaction logParsed logEvaluated accept reject pactEnv True hx
  where
    accept = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1
    logParsed pt = log ("Checking command with hash: " <> tshow (Pact._cmdHash pt)) Nothing
    logEvaluated _ = pure ()

deliver :: HandlerEffects m => DB -> HexString -> m (Response 'MTDeliverTx)
deliver pactEnv hx = runPactTransaction logParsed logEvaluated accept reject pactEnv False hx
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1
    logParsed pt = log ("Delivering command with hash: " <> tshow (Pact._cmdHash pt)) Nothing
    logEvaluated r = log "Pact result" (Just $ T.strip $ tshow $ Pact.pretty $ Pact._erOutput r)

runPactTransaction
  :: HandlerEffects m
  => (Pact.Command Text -> m ())
  -> (Pact.EvalResult -> m ())
  -> m a
  -> m a
  -> DB
  -> Bool
  -> HexString
  -> m a
runPactTransaction logParsed logEvaluated accept reject pactDbEnv shouldRollback hx = rejectOnError $ do
  txt <- decode hx
  pt <- parse txt
  lift $ logParsed pt
  r <- eval pt
  lift $ logEvaluated r

  where
    decode = withExceptT (\err -> ("Failed decode with error", Just $ tshow err))
      . liftEither . decodeHexString

    eval = execCmd pactDbEnv def shouldRollback

    parse txt = liftEither $ case T.readMaybe (T.unpack txt) of
      Nothing -> Left ("Failed to parse transaction", Nothing)
      Just v -> case Aeson.fromJSON v of
        Aeson.Error err -> Left ("Failed to parse JSON:", Just (T.pack err))
        Aeson.Success t -> Right t

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
