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

import Control.Concurrent.Async                         (async, cancel)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (ReaderT(..), runReaderT)
import Control.Monad.State.Strict                       (StateT)
import qualified Data.Aeson as Aeson
import Data.Functor                                     (void)
import Data.Maybe                                       (fromMaybe)
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))

import Prelude                                          hiding (head, log)

import Kadenamint.ABCI as ABCI
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint hiding (runNode, withNetwork)
import qualified Kadenamint.Tendermint as Tendermint
import Kadenamint.Tendermint.RPC

broadcastEnv :: Env
broadcastEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground cyan] . ("\n[RPC] " <>)
  }

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

runEverything :: IO ()
runEverything = timelineCoinContract

runNode :: InitializedNode -> IO ()
runNode = Tendermint.runNode runABCI

withNetwork
  :: Int
  -> (Text -> [InitializedNode] -> StateT Int IO ())
  -> IO ()
withNetwork = Tendermint.withNetwork runABCI

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
