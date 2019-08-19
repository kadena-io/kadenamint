{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader
import Data.Colour.SRGB (Colour, sRGB24)
import Data.Text (Text)
import qualified Data.Text as T
import Shelly
import System.Console.ANSI
import Prelude hiding (log)

import Network.ABCI.Server                              (serveApp)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger
import Network.ABCI.Types.Messages.Request              (CheckTx)

import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> liftIO runEverything
  , _backend_routeEncoder = backendRouteEncoder
  }

{- Process orchestration -}

runEverything :: IO ()
runEverything = do
  withAsync runNode $ \_ ->
    withAsync broadcastTransactions $ \_ ->
      runABCI

runNode :: IO ()
runNode = flip runReaderT nodeEnv $ do
  let
    deleteNetwork = run_ "rm" ["-rf", "~/.tendermint"]
    initNetwork = run_ "tendermint" ["init"]
    launchNode = run_ "tendermint" ["node"]
    resetNetwork = deleteNetwork *> initNetwork

  shelly resetNetwork
  log "Node has been reset"
  liftIO $ threadDelay $ seconds 1
  shelly launchNode

broadcastTransactions :: IO ()
broadcastTransactions = flip runReaderT broadcastEnv $ do
  liftIO $ threadDelay $ seconds 2
  log "About to broadcast transaction"
  shelly $ silently $ run_ "curl" ["localhost:26657/broadcast_tx_sync?tx=\"abc\""]

runABCI :: IO ()
runABCI = do
  _logger <- mkLogStdout -- too noisy
  serveApp $ transformApp transformHandler app
    where
      transformHandler :: EffectsT x -> IO x
      transformHandler = flip runReaderT abciEnv

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
  { _env_printer = sgrify [SetRGBColor Foreground cyan]
  }
nodeEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground red]
  }
abciEnv = Env
  { _env_printer = sgrify [SetRGBColor Foreground green]
  }

{- ABCI app -}

type EffectsT = ReaderT Env IO
type MonadEffects m = (MonadIO m, MonadReader Env m)

app :: App EffectsT
app = App $ \req -> case req of
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain _ -> pure def
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx a -> check a
  RequestDeliverTx _ -> pure def
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

check :: MonadEffects m => CheckTx -> m (Response 'MTCheckTx)
check x = do
  log $ tshow x
  pure def

{- Utils -}

tshow :: Show a => a -> Text
tshow = T.pack . show

log :: MonadEffects m => Text -> m ()
log txt = do
  p <- asks _env_printer
  liftIO $ putStrLn $ T.unpack $ p txt

sgrify :: [SGR] -> Text -> Text
sgrify codes txt = mconcat
  [ T.pack $ setSGRCode codes
  , txt
  , T.pack $ setSGRCode [Reset]
  ]

seconds :: Int -> Int
seconds = (*1e6)
