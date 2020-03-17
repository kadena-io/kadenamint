{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Kadenamint.Pact where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar)
import Control.Lens (preview, set, _Right, (&), (^.), (.~))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (Default (..))
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable(..), for_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import Data.Traversable (for)
import GHC.Generics (Generic)
import Servant (Get, Handler(..), JSON, err404, serve, throwError, (:<|>)(..), (:>))
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy, simpleHeaders)
import Numeric.Natural (Natural)

import Pact.ApiReq (ApiKeyPair(..), mkExec, mkKeyPairs)
import Pact.Interpreter (EvalResult(..), MsgData(..), PactDbEnv, defaultInterpreterState
                        , evalContinuation, evalExec, initRefStore, initSchema, mkSQLiteEnv, setupEvalEnv)
import Pact.Gas (freeGasEnv)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.SQLite (SQLite, SQLiteConfig(..))
import Pact.Types.Capability (CapSlot(..), SigCapability(..), capStack)
import Pact.Types.Command ( Command(..), CommandResult(..), PactResult(..), ParsedCode(..), Payload(..), ProcessedCommand(..), RequestKey
                          , cmdToRequestKey, cmdPayload, pPayload, pSigners, verifyCommand)
import Pact.Types.ChainMeta (PublicMeta, pmSender)
import Pact.Types.Crypto (PPKScheme(..), PrivateKeyBS(..), PublicKeyBS(..))
import Pact.Types.Logger (Loggers(newLogger), alwaysLog)
import Pact.Types.RPC (PactRPC(..), ContMsg(..), ExecMsg(..))
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.Hash (Hash(..))
import Pact.Types.Persistence (ExecutionMode(..))
import Pact.Types.Runtime (Gas(..), GasEnv(..), GasLimit(..), EvalState, PactError
                          , catchesPactError, evalCapabilities, permissiveNamespacePolicy)
import Pact.Server.API (ApiV1API)
import Pact.Server.PactService (fullToHashLogCr)
import Pact.Types.API (ListenResponse(..), ListenerRequest(..), Poll, PollResponses, RequestKeys(..), SubmitBatch(..))
import Pact.Types.Server (throwCmdEx)

import Kadenamint.Common

import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import Control.Monad.STM (atomically)

queueSize :: Natural
queueSize = 2000

addRequest :: TBQueue a -> a -> IO ()
addRequest q msg = atomically $ writeTBQueue q msg

getNextRequest :: TBQueue a -> IO a
getNextRequest q = atomically $ readTBQueue q

newtype DB = DB { unDB :: PactDbEnv (DbEnv SQLite) }

kadenamintGasEnv :: GasEnv
kadenamintGasEnv = freeGasEnv

initDb :: MonadIO m => FilePath -> m DB
initDb path = liftIO $ do
  pactDbEnv <- mkSQLiteEnv (newLogger alwaysLog "") True (SQLiteConfig path []) alwaysLog
  initSchema pactDbEnv
  pure $ DB pactDbEnv

applyCmd
  :: MonadIO m
  => DB
  -> (EvalState -> EvalState)
  -> Bool
  -> ((ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO a)
  -> Command Text
  -> m (Either PactError a)
applyCmd (DB pactDbEnv) stateF shouldRollback eval cmd = liftIO $ catchesPactError $ do
  case verifyCommand $ fmap T.encodeUtf8 cmd of
    ProcFail f -> error (show f)
    ProcSucc (c :: Command (Payload PublicMeta ParsedCode)) -> do
      let
        p = c ^. cmdPayload
        signers = p ^. pSigners
        interpreter = defaultInterpreterState stateF

        setupEvalEnv' execData = setupEvalEnv
          pactDbEnv
          Nothing
          (bool Transactional Local shouldRollback)
          (MsgData (fromMaybe Aeson.Null execData) Nothing (Hash "") signers)
          initRefStore
          kadenamintGasEnv
          permissiveNamespacePolicy
          noSPVSupport
          def
          def

        withExec (ExecMsg code execData) = evalExec interpreter (setupEvalEnv' (Just execData)) code
        withCont cont = evalContinuation interpreter (setupEvalEnv' Nothing) cont

      todo Todo_SerializeEval $ eval withExec withCont $ p ^. pPayload

runCmd :: (ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO EvalResult
runCmd withExec withCont = \case
  Exec x -> withExec x
  Continuation c -> withCont c

runExec :: (ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO EvalResult
runExec withExec _ = \case
  Exec x -> withExec x
  _ -> throwCmdEx "local continuations not supported"

mkExec' :: MonadIO m => Text -> Maybe Text -> Maybe [SigCapability] -> m (Command Text)
mkExec' code sender caps = liftIO $ do
  senderKey <- for sender $ \s -> stockKey s caps
  kps <- mkKeyPairs $ toList senderKey

  mkExec
    (T.unpack code)
    Aeson.Null
    (def & maybe id (pmSender .~) sender)
    kps
    Nothing
    Nothing

stockKey :: Text -> Maybe [SigCapability] -> IO ApiKeyPair
stockKey s caps = do
    let Right (Y.Object o) = Y.decodeEither' stockKeyFile
        Just (Y.Object kp) = HM.lookup s o
        Just (Y.String pub) = HM.lookup "public" kp
        Just (Y.String priv) = HM.lookup "secret" kp
        mkKeyBS = fst . B16.decode . T.encodeUtf8
    return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) caps

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/devnet/keys.yaml")

initCapabilities :: [CapSlot SigCapability] -> EvalState -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs

pollHandler :: Poll -> Handler PollResponses
pollHandler _ = todo Todo_ImplementEndpoint $ throwError err404

listenHandler :: RequestResults -> ListenerRequest -> Handler ListenResponse
listenHandler requestResults (ListenerRequest reqKey) = liftIO $ do
  slot <- ensureMVarExists requestResults reqKey
  ListenResponse <$> readMVar slot

sendHandler :: (Command Text -> IO ()) -> SubmitBatch -> Handler RequestKeys
sendHandler rpc (SubmitBatch cmds) = liftIO $ do
  for_ cmds rpc
  pure $ RequestKeys $ fmap cmdToRequestKey cmds

localHandler :: MonadIO m =>  DB -> Command Text -> m (CommandResult Hash)
localHandler db cmd = mkCommandResult cmd <$> applyCmd db id True runExec cmd

data RequestMsg = RequestMsg
  { _requestMsg_eval :: ((ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO EvalResult)
  , _requestMsg_command :: Command Text
  }

type RequestResults = IORef (HM.HashMap RequestKey (MVar (CommandResult Hash)))

ensureMVarExists :: MonadIO m => RequestResults -> RequestKey -> m (MVar (CommandResult Hash))
ensureMVarExists requestResults reqKey = liftIO $ do
  new <- newEmptyMVar
  atomicModifyIORef' requestResults $ \rrs ->
    case HM.lookup reqKey rrs of
      Nothing -> (HM.insert reqKey new rrs, new)
      Just s -> (rrs, s)

mkCommandResult :: Command Text -> (Either PactError EvalResult) -> CommandResult Hash
mkCommandResult cmd er = CommandResult
  { _crReqKey = cmdToRequestKey cmd
  , _crTxId = _erTxId <=< preview _Right $ er
  , _crResult = PactResult $ fmap lastOutput er
  , _crGas = either (const $ maxGas kadenamintGasEnv) _erGas er
  , _crLogs = fullToHashLogCr <$> evalLogs er
  , _crContinuation = noCont
  , _crMetaData = noMetadata
  }
  where
    maxGas (GasEnv (GasLimit g) _ _) = Gas $ fromIntegral g
    lastOutput = todo Todo_NonemptyPactCode $ last . _erOutput
    evalLogs = assume Assumption_OnlyEvalLogs $ fmap _erLogs . preview _Right
    noCont = assume Assumption_NoLocalContinuations Nothing
    noMetadata = todo Todo_PlatformMetadata Nothing

infoHandler :: Version -> Handler NodeInfo
infoHandler v = pure $ NodeInfo
  { nodeVersion = v
  , nodeApiVersion = todo Todo_Versioning "0.0"
  , nodeChains = todo Todo_Naming ["0"]
  , nodeNumberOfChains = 1
  }

data Version
  = Version_Devnet_00
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Version where
  toJSON Version_Devnet_00 = "devnet00"

instance FromJSON Version where
  parseJSON (Aeson.String "devnet00") = pure Version_Devnet_00
  parseJSON _ = fail "invalid kadenamint version"

data NodeInfo = NodeInfo
  { nodeVersion :: Version
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  , nodeNumberOfChains :: !Int
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON NodeInfo
instance ToJSON NodeInfo

type PactAPI = "chainweb" :> "0.0" :> "devnet00" :> "chain" :> "0" :> "pact" :> ApiV1API
type ChainweaverAPI = "info" :> Get '[JSON] NodeInfo
type KadenamintAPI = ChainweaverAPI :<|> PactAPI

kadenamintApi :: Proxy KadenamintAPI
kadenamintApi = Proxy

runApiServer :: DB -> RequestResults -> (Command Text -> IO ()) -> IO ()
runApiServer db rrs broadcast = do
  let
    port = assume Assumption_FreePort 8081
    chainweaverApiHandlers = infoHandler Version_Devnet_00
    pactApiHandlers = sendHandler broadcast
                 :<|> pollHandler
                 :<|> listenHandler rrs
                 :<|> localHandler db

  run port $ kadenamintCors $ serve kadenamintApi $ chainweaverApiHandlers :<|> todo Todo_Versioning pactApiHandlers

-- Simple cors with actually simpleHeaders which includes content-type.
kadenamintCors :: Middleware
kadenamintCors = cors . const . Just $ simpleCorsResourcePolicy
    { corsRequestHeaders = simpleHeaders
    }
