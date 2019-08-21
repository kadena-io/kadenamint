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
import Data.Conduit.Network                             (HostPreference, ServerSettings, serverSettings)
import Data.Foldable (for_)
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
import Network.ABCI.Server                              (serveAppWith)
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
data Actor
  = Actor_Node
  | Actor_Broadcast
  deriving (Eq, Ord, Enum, Bounded, Show)

runEverything :: IO ()
runEverything = do
  withAsync (runActor Actor_Node) $ \_ ->
    withAsync (runActor Actor_Broadcast) $ \_ ->
      runABCI

timelineEntries :: [(Int, Actor, ReaderT Env IO ())]
timelineEntries =
  let
    deleteNetwork = run_ "rm" ["-rf", tendermintHome]
    initNetwork = tendermint "init" []

    launchNode = shelly $ tendermintNode
    resetNetwork = do
      shelly deleteNetwork
      shelly initNetwork
      log "Node has been reset"
  in
    [ (seconds 0, Actor_Node, resetNetwork)
    , (seconds 0, Actor_Node, launchNode)
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction "(+ 1 2)")
    , (seconds 2, Actor_Broadcast, broadcastPactTransaction "(+ 1 (* a 3))")
    ]

runActor :: Actor -> IO ()
runActor actor = flip runReaderT nodeEnv $ do
  for_ timelineEntries $ \(entryDelay, entryActor, entryAction) -> do
    when (actor == entryActor) $ do
      liftIO $ threadDelay entryDelay
      entryAction

broadcastPactTransaction :: MonadIO m => Text -> m ()
broadcastPactTransaction pt = flip runReaderT broadcastEnv $ do
  log $ "Broadcasting code:\t" <> pt
  case T.decodeUtf8' $ view strict $ toLazyByteString $ encodePath ["broadcast_tx_sync"] [("tx", Just (T.encodeUtf8 $ doubleQuotes pt))] of
    Left err -> do
      log "Failed encoding of transaction with error:"
      log $ tshow err
      fatal
    Right pathAndQuery -> do
      let url = mkAddress defaultTendermintRPCHostPort <> pathAndQuery
      log $ "Broadcasting at:\t" <> url
      shelly $ silently $ run_ "curl" [url]

runABCI :: IO ()
runABCI = do
  _logger <- mkLogStdout -- too noisy
  serveAppWith (mkServerSettings defaultABCIAppHostPort) mempty $ transformApp transformHandler app
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


{- Tendermint -}
tendermint :: Text -> [Text] -> Sh ()
tendermint tmCmd cmdArgs = run_ "tendermint" $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", tendermintHome]

tendermintNode :: Sh ()
tendermintNode = tendermint "node"
  [ "--p2p.laddr", mkAddress defaultTendermintP2PHostPort
  , "--rpc.laddr", "tcp://" <> mkAddress defaultTendermintRPCHostPort & _UPSTREAM_ "incoherent with other address flags"
  , "--proxy_app", mkAddress defaultABCIAppHostPort
  ]

tendermintHome :: IsString a => a
tendermintHome = "./.tendermint"

defaultTendermintP2PHostPort :: (Text, Int)
defaultTendermintP2PHostPort = ("0.0.0.0", 26656)

defaultTendermintRPCHostPort :: (Text, Int)
defaultTendermintRPCHostPort = ("127.0.0.1", 26657)

{- ABCI app -}
defaultABCIAppHostPort :: IsString a => (a, Int)
defaultABCIAppHostPort = ("127.0.0.1", 26658)

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
      output <- shelly $ silently $ errExit False $ shecked $ run "echo" [p] -|- run "pact" []
      case output of
        Left err -> reject $ "Pact error:\n" <> err
        Right r -> accept $ "Pact result:\n" <> T.strip r

{- Utils -}
type HostPort a = IsString a => (a, Int)

shecked :: Sh a -> Sh (Either Text a)
shecked sh = do
  output <- sh
  lastExitCode >>= \case
    0 -> pure $ Right output
    _ -> Left <$> lastStderr

mkAddress :: HostPort Text -> Text
mkAddress (host, port) = host <> ":" <> tshow port

mkServerSettings :: HostPort HostPreference -> ServerSettings
mkServerSettings (host, port) = serverSettings port host

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
decodeHexString (HexString bs) = T.decodeUtf8' bs & _TODO_ "make sure this is the right decoding"

{- Issue tracking -}
_UPSTREAM_ :: Text -> a -> a
_UPSTREAM_ _ = id

_TODO_ :: Text -> a -> a
_TODO_ _ = id
