{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Kadenamint.ABCI where

import Control.Lens                                     ((&), (^.), (.~))
import Control.Monad                                    ((>=>))
import Control.Monad.Except                             (MonadError(..), ExceptT(..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.Trans.Class                        (lift)
import qualified Data.Aeson as Aeson
import Data.Conduit.Network                             (serverSettings)
import Data.Default                                     (Default(..))
import Data.Functor                                     (void)
import Data.Maybe                                       (fromMaybe)
import Data.String                                      (IsString(..))
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T          hiding (replace)
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))
import Text.Read as T                                   (readMaybe)

import Prelude                                          hiding (head, log)

import Data.ByteArray.HexString                         (HexString(..))
import Network.ABCI.Server                              (serveAppWith)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Types.Messages.Request              (CheckTx(..), DeliverTx(..), InitChain(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _deliverTxCode, _exceptionError)
import qualified Pact.Interpreter as Pact
import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Pretty as Pact

import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint

abciEnv :: Text -> Env
abciEnv moniker = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground green] $ "\n[ABCI] Node: " <> moniker <> " | " <> x
  }

runABCI :: InitializedNode -> IO ()
runABCI n = do
  let cfg = n ^. initializedNode_config
      home = n ^. initializedNode_home

  pactDbEnv <- initDb $ T.unpack home <> "/pact-db"

  let
    env = abciEnv $ _config_moniker cfg

    (_protocol, rest) = cleave "://" (cfg ^. config_proxyApp)
    (host, port) = cleave ":" rest
    port' = fromMaybe (error "parsing error") $ T.readMaybe (T.unpack port)
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
    logParsed h = log ("Checking command with hash: " <> h) Nothing
    logEvaluated _ = pure ()

deliver :: HandlerEffects m => DB -> HexString -> m (Response 'MTDeliverTx)
deliver pactEnv hx = runPactTransaction logParsed logEvaluated accept reject pactEnv False hx
  where
    accept = pure def
    reject = pure $ ResponseDeliverTx $ def & _deliverTxCode .~ 1
    logParsed h = log ("Delivering command with hash: " <> h) Nothing
    logEvaluated r = log "Pact result" (Just r)

runPactTransaction
  :: HandlerEffects m
  => (Text -> m ())
  -> (Text -> m ())
  -> m a
  -> m a
  -> DB
  -> Bool
  -> HexString
  -> m a
runPactTransaction logParsed logEvaluated accept reject pactDbEnv shouldRollback hx = rejectOnError $ do
  txt <- decode hx
  pt <- parse txt
  lift $ logParsed $ tshow $ Pact._cmdHash pt
  r <- eval pt
  lift $ logEvaluated $ T.strip $ tshow $ Pact.pretty $ Pact._erOutput r

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

decodeHexString :: HexString -> Either T.UnicodeException Text
decodeHexString (HexString bs) = T.decodeUtf8' bs & _TODO_ "make sure this is the right decoding"
