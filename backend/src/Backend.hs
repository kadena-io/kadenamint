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
import Control.Lens                                     (strict, view, (&), (.~))
import Control.Monad.Except                             (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader
import Data.Binary.Builder
import Data.Colour.SRGB (Colour, sRGB24)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Network.HTTP.Types
import Shelly
import System.Console.ANSI
import Prelude hiding (log)

import Data.ByteArray.HexString (HexString(..))
import qualified Data.ByteArray.HexString as Hex
import Network.ABCI.Server                              (serveApp)
import Network.ABCI.Server.App                          (App(..), Request(..), Response(..), MessageType(..), transformApp)
import Network.ABCI.Server.Middleware.RequestLogger
import Network.ABCI.Types.Messages.Request              (CheckTx(..))
import Network.ABCI.Types.Messages.Response             (_checkTxCode, _exceptionError)

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
  shelly launchNode

broadcastTransactions :: IO ()
broadcastTransactions = do
  liftIO $ threadDelay $ seconds 2
  broadcastPactTransaction "(+ 1 2)"
  liftIO $ threadDelay $ seconds 2
  broadcastPactTransaction "(+ 1 (* a 3))"

broadcastPactTransaction :: Text -> IO ()
broadcastPactTransaction pt = flip runReaderT broadcastEnv $ do
  log $ "Broadcasting code:\t" <> pt
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath ["broadcast_tx_sync"] [("tx", Just (T.encodeUtf8 $ doubleQuotes pt))] of
    Left err -> do
      log "Failed encoding of transaction with error:"
      log $ tshow err
      fatal
    Right pathAndQuery -> do
      let url = "localhost:26657" <> pathAndQuery
      log $ "Broadcasting at:\t" <> url
      shelly $ silently $ run_ "curl" [url]

runABCI :: IO ()
runABCI = do
  _logger <- mkLogStdout -- too noisy
  serveApp $ transformApp transformHandler app
    where
      transformHandler :: EffectsT (Response t) -> IO (Response t)
      transformHandler er = do
        x <- runExceptT $ flip runReaderT abciEnv $ er
        case x of
          Right r -> pure r
          Left l -> pure $ ResponseException $ def
            & _exceptionError .~ l

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

type Err = Text
type EffectsT = ReaderT Env (ExceptT Err IO)
type MonadEffects m = (MonadIO m, MonadError Err m, MonadReader Env m)

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
check (CheckTx x) = do
  let accept msg = do
        log msg
        pure def
      reject msg = do
        log msg
        pure $ ResponseCheckTx $ def & _checkTxCode .~ 1

  log $ "Decoding:\t" <> Hex.toText x
  case decodeHexString x of
    Left err -> reject $ "Failed decode with error: " <> tshow err
    Right p -> do
      log $ "Decoded:\t" <> p
      output <- shelly $ silently $ errExit False $ do
        output <- run "echo" [p] -|- run "pact" []
        code <- lastExitCode
        case code of
          0 -> pure $ Right output
          _ -> Left <$> lastStderr
      case output of
        Left err -> reject $ "Pact error:\n" <> err
        Right r -> accept $ "Pact result:\n" <> T.strip r

{- Utils -}

tshow :: Show a => a -> Text
tshow = T.pack . show

log :: (MonadIO m, MonadReader Env m) => Text -> m ()
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

fatal :: m ()
fatal = error "fatal error"

doubleQuotes :: (IsString a, Semigroup a) => a -> a
doubleQuotes t = "\"" <> t <> "\""

decodeHexString :: HexString -> Either T.UnicodeException Text
decodeHexString (HexString bs) = _TODO_ "make sure this is the right decoding" $ T.decodeUtf8' bs

_TODO_ :: Text -> a -> a
_TODO_ _ = id
