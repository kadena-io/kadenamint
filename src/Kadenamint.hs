{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kadenamint where

import Control.Concurrent.Async                         (async, cancel, withAsync)
import Control.Lens                                     (makeLenses, (^.))
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (ReaderT(..), runReaderT)
import qualified Data.Aeson as Aeson
import Data.Decimal                                     (Decimal)
import Data.Functor                                     (void)
import Data.IORef                                       (newIORef)
import Data.Text                                        (Text)
import qualified Data.Text as T
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))

import Prelude                                          hiding (head, log)

import Pact.Types.Capability
import Pact.Types.Command

import Kadenamint.ABCI as ABCI
import Kadenamint.Coin
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint
import Kadenamint.Tendermint.RPC

data KadenamintNode = KadenamintNode
  { _kadenamintNode_tendermint :: TendermintNode
  , _kadenamintNode_pactAPIPort :: Word
  }

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
  timelineCoinContract

withNode :: MonadIO m => KadenamintNode -> m ()
withNode kn = liftIO $ do
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
runKadenamintNodeDir = runNodeDir mkKadenamintNode _kadenamintNode_tendermint withNode

runKadenamintNode :: MonadIO m => KadenamintNode -> m ()
runKadenamintNode = runNode _kadenamintNode_tendermint withNode

withKadenamintNetwork
  :: Word
  -> (Text -> [KadenamintNode] -> IO ())
  -> IO ()
withKadenamintNetwork = withNetwork mkKadenamintNode _kadenamintNode_tendermint withNode

showBalancesTx :: MonadIO m => KadenamintNode -> m ()
showBalancesTx = broadcastPact showBalances

showBalanceTx :: MonadIO m => Text -> KadenamintNode -> m ()
showBalanceTx acct = broadcastPact ("(coin.get-balance '" <> acct <> ")")

transferTx :: MonadIO m => Text -> Text -> Decimal -> KadenamintNode -> m ()
transferTx from to amount = broadcastPactSigned (Just from) (Just [mkTransferCapability from to amount]) (transfer from to amount <> showBalances)

timelineCoinContract :: IO ()
timelineCoinContract = withKadenamintNetwork 2 $ \root -> \case
  [n0, n1] -> do
    sleep 4
    showBalancesTx n1

    sleep 2
    n3 <- addKadenamintNode (root <> "/nodeX") "nodeX" extraNodePorts n0
    a3 <- liftIO $ async $ runKadenamintNode n3

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
    void $ liftIO $ async $ runKadenamintNode n3

  _ -> impossible

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
    log ("Broadcasting pact code to node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow code)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd

broadcastPactCmd :: MonadIO m => KadenamintNode -> Command Text -> m ()
broadcastPactCmd kn cmd = do
  let
    tn = _kadenamintNode_tendermint kn
    cfg = _tendermintNode_config tn
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact command to node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow cmd)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd
