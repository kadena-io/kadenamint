{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kadenamint.Tendermint.RPC where

import Control.Lens                                     (strict, view, (&))
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..))
import Data.Binary.Builder                              (toLazyByteString)
import Data.ByteArray.Encoding                          (Base(Base16), convertToBase)
import Data.Text                                        (Text)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types                               (encodePath)
import Shelly                                           (shelly, silently, run_)

import Prelude                                          hiding (log)

import Kadenamint.Common

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
