{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Kadenamint.Tendermint.RPC where

import Control.Exception                                (SomeException, handle)
import Control.Lens                                     ((&))
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..))
import qualified Data.ByteArray.Base64String as Base64
import Data.Text                                        (Text)
import qualified Data.Text.Encoding as T

import Prelude                                          hiding (log)

import Network.Tendermint.Client

import Kadenamint.Common

broadcastTransaction :: (MonadIO m, MonadReader Env m) => Text -> Int -> Text -> m ()
broadcastTransaction host port t = liftIO $ do
  let
    cfg = defaultConfig (T.encodeUtf8 host) port
    broadcast = runTendermintM cfg $ broadcastTxCommit $ RequestBroadcastTxCommit $ Base64.fromBytes $ T.encodeUtf8 t

  r <- fmap Right broadcast
    & handle @SomeException (pure . Left)
  case r of
    Left err -> print err
    Right _ -> pure ()
