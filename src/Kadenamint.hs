{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Kadenamint where

import Control.Concurrent.Async                         (async, cancel, withAsync)
import Control.Lens                                     ((^.))
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

broadcastEnv :: Env
broadcastEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground cyan] . ("\n[RPC] " <>)
  }

runEverything :: IO ()
runEverything = do
  initProcess
  timelineCoinContract

withNode :: MonadIO m => InitializedNode -> m ()
withNode n = liftIO $ do
  let home = n ^. initializedNode_home
      (_, proxyAppPort) = unsafeHostPortFromURI $ n ^. initializedNode_config . config_proxyApp
  rrs <- newIORef mempty
  pactDbEnv <- initDb $ T.unpack home <> "/pact-db"
  withAsync (runApiServer pactDbEnv rrs (broadcastPactCmd n) (proxyAppPort + 1)) $ \_ -> runABCI pactDbEnv rrs n

runKadenamintNodeDir :: MonadIO m => Text -> m ()
runKadenamintNodeDir = runNodeDir withNode

runKadenamintNode :: MonadIO m => InitializedNode -> m ()
runKadenamintNode = runNode withNode

withKadenamintNetwork
  :: Word
  -> (Text -> [InitializedNode] -> IO ())
  -> IO ()
withKadenamintNetwork = withNetwork withNode

showBalancesTx :: MonadIO m => InitializedNode -> m ()
showBalancesTx = broadcastPact showBalances

showBalanceTx :: MonadIO m => Text -> InitializedNode -> m ()
showBalanceTx acct = broadcastPact ("(coin.get-balance '" <> acct <> ")")

transferTx :: MonadIO m => Text -> Text -> Decimal -> InitializedNode -> m ()
transferTx from to amount = broadcastPactSigned (Just from) (Just [mkTransferCapability from to amount]) (transfer from to amount <> showBalances)

timelineCoinContract :: IO ()
timelineCoinContract = withKadenamintNetwork 2 $ \root -> \case
  [n0, n1] -> do
    sleep 4
    showBalancesTx n1

    sleep 2
    n3 <- addNode (root <> "/nodeX") "nodeX" extraNodePorts n0
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

timelineRepl :: IO ()
timelineRepl = withKadenamintNetwork 2 $ \_ -> \case
  [n0, n1] -> do
    sleep 3 *> broadcastPact "(+ 1 2)" n0
    sleep 2 *> broadcastPact "(+ 1 2)" n1
  _ -> impossible

broadcastPact :: MonadIO m => Text -> InitializedNode -> m ()
broadcastPact = broadcastPactSigned Nothing Nothing

broadcastPactSigned :: MonadIO m => Maybe Text -> Maybe [SigCapability] -> Text -> InitializedNode -> m ()
broadcastPactSigned sender caps code n = do
  let
    cfg = _initializedNode_config n
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  cmd <- mkExec' code sender caps

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact code to node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow code)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd

broadcastPactCmd :: MonadIO m => InitializedNode -> Command Text -> m ()
broadcastPactCmd n cmd = do
  let
    cfg = _initializedNode_config n
    (host, port) = unsafeHostPortFromURI $ _configRPC_laddr $ _config_rpc cfg

  flip runReaderT broadcastEnv $ do
    log ("Broadcasting pact command to node #" <> _config_moniker cfg <> " at " <> host <> ":" <> tshow port) (Just $ tshow cmd)
    broadcastTransaction host port $ tshow $ Aeson.toJSON cmd
