{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kadenamint where

import Control.Concurrent.Async                         (async, cancel, withAsync)
import Control.Lens                                     (makeLenses, (^.), (<&>))
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (ReaderT(..), runReaderT)
import qualified Data.Aeson as Aeson
import Data.Decimal                                     (Decimal)
import Data.Functor                                     (void)
import Data.IORef                                       (newIORef)
import Data.Text                                        (Text)
import qualified Data.Text as T
import Network.HTTP.Client                              (defaultManagerSettings, newManager)
import Servant.Client                                   (BaseUrl(..),  Scheme(Http), mkClientEnv, runClientM)
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))

import Prelude                                          hiding (head, log)

import Pact.Types.Capability                            (SigCapability)
import Pact.Types.Command                               (Command(..), CommandResult(..), PactResult(..))
import Pact.Types.Pretty                                (pretty)

import Kadenamint.ABCI as ABCI
import Kadenamint.Coin
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint
import Kadenamint.Tendermint.RPC

data KadenamintNode = KadenamintNode
  { _kadenamintNode_tendermint :: TendermintNode
  , _kadenamintNode_pactAPIPort :: Word
  } deriving (Eq, Ord, Show)

mkKadenamintNode :: TendermintNode -> KadenamintNode
mkKadenamintNode tn = KadenamintNode tn apiPort
  where
    apiPort = proxyAppPort + 1
    (_, proxyAppPort) = unsafeHostPortFromURI $ tn ^. tendermintNode_config . config_proxyApp

makeLenses ''KadenamintNode

broadcastEnv :: Env
broadcastEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground cyan] . ("\n[RPC] " <>)
  }

runEverything :: IO ()
runEverything = do
  initProcess
  withLocalKadenamintNetwork 3 $ \root -> \case
    [n0, n1, _n2] -> timelineCoinContract root n0 n1
    _ -> impossible

withKadenamintNode :: MonadIO m => KadenamintNode -> m ()
withKadenamintNode kn = liftIO $ do
  let tn = _kadenamintNode_tendermint kn
      home = tn ^. tendermintNode_home
      (_, proxyAppPort) = unsafeHostPortFromURI $ tn ^. tendermintNode_config . config_proxyApp
  rrs <- newIORef mempty
  pactDbEnv <- initDb $ T.unpack home <> "/pact-db"
  withAsync (runApiServer pactDbEnv rrs (broadcastPactCmd kn) (proxyAppPort + 1)) $ \_ -> runABCI pactDbEnv rrs tn

addKadenamintNode :: MonadIO m => Text -> Text -> NodePorts -> KadenamintNode -> m KadenamintNode
addKadenamintNode home moniker ports preExistingNode = mkKadenamintNode <$> addTendermintNode home moniker ports (_kadenamintNode_tendermint preExistingNode)

loadKadenamintNode :: MonadIO m => Text -> m KadenamintNode
loadKadenamintNode = fmap mkKadenamintNode . loadTendermintNode

runKadenamintNodeDir :: MonadIO m => Text -> m ()
runKadenamintNodeDir = runNodeDir mkKadenamintNode _kadenamintNode_tendermint withKadenamintNode

runKadenamintNode :: MonadIO m => KadenamintNode -> m ()
runKadenamintNode = runNode _kadenamintNode_tendermint withKadenamintNode

withThrowawayKadenamintNetwork :: Word -> (Text -> [KadenamintNode] -> IO ()) -> IO ()
withThrowawayKadenamintNetwork size f = withTempDir $ \x -> withKadenamintNetwork x size f

withLocalKadenamintNetwork :: Word -> (Text -> [KadenamintNode] -> IO ()) -> IO ()
withLocalKadenamintNetwork size f = withCurrentDir $ \x -> withKadenamintNetwork (x <> "/.network") size f

withKadenamintNetwork
  :: Text
  -> Word
  -> (Text -> [KadenamintNode] -> IO ())
  -> IO ()
withKadenamintNetwork root size = withNetwork root $ AppNetwork
  { _appNetwork_toAppNode = mkKadenamintNode
  , _appNetwork_fromAppNode = _kadenamintNode_tendermint
  , _appNetwork_withNode = withKadenamintNode
  , _appNetwork_size = size
  }

showBalancesTx :: MonadIO m => KadenamintNode -> m ()
showBalancesTx = broadcastPact showBalances

showBalanceTx :: MonadIO m => Text -> KadenamintNode -> m ()
showBalanceTx acct = broadcastPact ("(coin.get-balance '" <> acct <> ")")

transferTx :: MonadIO m => Text -> Text -> Decimal -> KadenamintNode -> m ()
transferTx from to amount = broadcastPactSigned (Just from) (Just [mkTransferCapability from to amount]) (transfer from to amount <> showBalances)

timelineCoinContract :: Text -> KadenamintNode -> KadenamintNode -> IO ()
timelineCoinContract root n0 n1 = do
    sleep 4
    showBalancesTx n1

    sleep 4
    n3 <- addKadenamintNode (root <> "/nodeX") "nodeX" extraNodePorts n0
    a3 <- liftIO $ async $ runKadenamintNode n3

    sleep 4
    showBalancesTx n3

    sleep 4
    transferTx "sender00" "sender01" 1 n3

    sleep 4
    liftIO $ cancel a3
    flip runReaderT (coreEnv Nothing) $ log "Stopping nodeX" Nothing

    sleep 4
    transferTx "sender00" "sender02" 1 n0

    sleep 4
    void $ liftIO $ async $ runKadenamintNode n3

broadcastPact :: MonadIO m => Text -> KadenamintNode -> m ()
broadcastPact = broadcastPactSigned Nothing Nothing

broadcastPactSigned :: MonadIO m => Maybe Text -> Maybe [SigCapability] -> Text -> KadenamintNode -> m ()
broadcastPactSigned sender caps code kn = do
  let
    tn = _kadenamintNode_tendermint kn
    cfg = _tendermintNode_config tn
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  cmd <- mkExec' code sender caps

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact code via node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow code)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd

broadcastPactCmd :: MonadIO m => KadenamintNode -> Command Text -> m ()
broadcastPactCmd kn cmd = do
  let
    tn = _kadenamintNode_tendermint kn
    cfg = _tendermintNode_config tn
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact command via node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow cmd)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd

localCall :: MonadIO m => Text -> KadenamintNode -> m String
localCall code kn = do
  m <- liftIO $ newManager defaultManagerSettings
  cmd <- mkExec' code Nothing Nothing

  let
    apiPort = fromEnum $ _kadenamintNode_pactAPIPort kn
    nodeUrl = BaseUrl Http "localhost" apiPort ""
    env = mkClientEnv m nodeUrl

    tn = _kadenamintNode_tendermint kn
    cfg = _tendermintNode_config tn
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  flip runReaderT broadcastEnv $ do
    log ("Sending pact command to /local endpoint of node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow cmd)

  liftIO $ runClientM (localEndpoint cmd) env <&> \case
    Left err -> show err
    Right cr -> case _crResult cr of
      PactResult (Left err) -> show err
      PactResult (Right er) -> show $ pretty $ er
