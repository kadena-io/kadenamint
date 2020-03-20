{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Kadenamint.ABCI where

import Control.Concurrent.MVar                          (putMVar)
import Control.Lens                                     ((&), (^.), (.~), (??))
import Control.Monad                                    ((>=>))
import Control.Monad.Except                             (ExceptT(..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), runReaderT)
import qualified Data.Aeson as Aeson
import Data.Conduit.Network                             (serverSettings)
import Data.Default                                     (Default(..))
import Data.String                                      (IsString(..))
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))
import Text.Read as T                                   (readMaybe)

import Prelude                                          hiding (head, log)

import Data.ByteArray.Base64String                      (Base64String(..))
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..), InitChain(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.Interpreter as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.Pretty as Pact

import Kadenamint.Coin
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint

abciEnv :: Text -> Env
abciEnv moniker = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground green] $ "\n[ABCI] Node: " <> moniker <> " | " <> x
  }

runABCI :: DB -> RequestResults -> TendermintNode -> IO ()
runABCI pactDbEnv rrs n = do
  let
    cfg = n ^. tendermintNode_config

    env = abciEnv $ _config_moniker cfg
    (host, port) = unsafeHostPortFromURI $ cfg ^. config_proxyApp

    transformHandler :: HandlerT (Response t) -> IO (Response t)
    transformHandler er = do
      x <- runExceptT $ runReaderT er env
      case x of
        Right r -> pure r
        Left l -> pure $ ResponseException $ def
          & _exceptionError .~ l

  serveAppWith (serverSettings (fromEnum port) (fromString . T.unpack $ host)) mempty
    $ transformApp transformHandler
    $ app pactDbEnv rrs

type Err = Text
type HandlerT = ReaderT Env (ExceptT Err IO)
type HandlerEffects m = (MonadIO m, MonadReader Env m)

app :: HandlerEffects m => DB -> RequestResults -> App m
app pactDbEnv rrs = App $ \case
  RequestEcho _ -> pure def
  RequestFlush _ -> pure def
  RequestInfo _ -> pure def
  RequestSetOption _ -> pure def
  RequestInitChain ic -> initChain pactDbEnv ic
  RequestQuery _ -> pure def
  RequestBeginBlock _ -> pure def
  RequestCheckTx (CheckTx hx) -> check pactDbEnv hx
  RequestDeliverTx (DeliverTx hx) -> deliver pactDbEnv rrs hx
  RequestEndBlock _ -> pure def
  RequestCommit _ -> pure def

initChain :: HandlerEffects m => DB -> InitChain -> m (Response 'MTInitChain)
initChain pactDbEnv _ic = abortOnError $ applyCoinGenesis pactDbEnv (log ?? Nothing)
  where
    abortOnError = runExceptT >=> \case
      Right _termName -> pure def
      Left err -> do
        log "Init chain failed" (Just err)
        error $ T.unpack err

check :: HandlerEffects m => DB -> Base64String -> m (Response 'MTCheckTx)
check pactEnv hx = runPactTransaction logParsed logEvaluated accept reject pactEnv True hx
  where
    accept _ _ = pure def
    reject = pure $ ResponseCheckTx $ def & _checkTxCode .~ 1
    logParsed h = log ("Checking command with hash: " <> h) Nothing
    logEvaluated _ = pure ()

deliver :: HandlerEffects m => DB -> RequestResults -> Base64String -> m (Response 'MTDeliverTx)
deliver pactEnv rrs hx = runPactTransaction logParsed logEvaluated accept reject pactEnv False hx
  where
    accept cmd cmdRes = liftIO $ do
      slot <- ensureMVarExists rrs (Pact.cmdToRequestKey cmd)
      putMVar slot cmdRes
      pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1
    logParsed h = log ("Delivering command with hash: " <> h) Nothing
    logEvaluated r = log "Pact result" (Just r)

runPactTransaction
  :: HandlerEffects m
  => (Text -> m ())
  -> (Text -> m ())
  -> (Pact.Command Text -> Pact.CommandResult Pact.Hash -> m a)
  -> m a
  -> DB
  -> Bool
  -> Base64String
  -> m a
runPactTransaction logParsed logEvaluated accept reject pactDbEnv shouldRollback hx = do
  runExceptT (decode hx >>= parse) >>= \case
    Left (h,b) -> log ("Rejecting transaction - " <> h) b *> reject
    Right cmd -> do
      logParsed $ tshow $ Pact._cmdHash cmd
      r <- eval cmd
      case r of
        Left err -> logEvaluated $ T.strip $ tshow err
        Right res -> logEvaluated $ T.strip $ tshow $ Pact.pretty $ Pact._erOutput res
      accept cmd (mkCommandResult cmd r)

  where
    decode = withExceptT (\err -> ("Failed decode with error", Just $ tshow err))
      . liftEither . decodeBase64String

    eval = applyCmd pactDbEnv id shouldRollback runCmd

    parse txt = liftEither $ case T.readMaybe (T.unpack txt) of
      Nothing -> Left ("Failed to parse transaction", Nothing)
      Just v -> case Aeson.fromJSON v of
        Aeson.Error err -> Left ("Failed to parse JSON:", Just (T.pack err))
        Aeson.Success t -> Right t

decodeBase64String :: Base64String -> Either T.UnicodeException Text
decodeBase64String (Base64String bs) = T.decodeUtf8' bs & _TODO_ "make sure this is the right decoding"
