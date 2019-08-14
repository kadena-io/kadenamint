{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class                           (MonadIO(..))

import Network.ABCI.Server                              (serveApp)
import Network.ABCI.Server.App                          (App(..), Request(..))
import Network.ABCI.Server.Middleware.RequestLogger     (mkLogStdoutDev)

import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> liftIO runABCI
  , _backend_routeEncoder = backendRouteEncoder
  }

runABCI :: IO ()
runABCI = do
  print "Launching application"
  logger <- mkLogStdoutDev
  serveApp $ logger app

type EffectsT = IO

app :: App EffectsT
app = App $ \req -> case req of
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain _ -> pure def
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx _ -> pure def
  RequestDeliverTx _ -> pure def
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def
