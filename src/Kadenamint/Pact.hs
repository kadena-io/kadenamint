{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Kadenamint.Pact where

import Control.Lens (preview, set, _Right, (&), (^.), (.~))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (Default (..))
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import GHC.Generics
import Servant (Get, Handler(..), JSON, err404, serve, throwError, (:<|>)(..), (:>))
import Network.Wai.Handler.Warp (run)

import Pact.ApiReq (ApiKeyPair(..), mkApiReq, mkExec, mkKeyPairs)
import Pact.Interpreter (EvalResult(..), MsgData(..), PactDbEnv, defaultInterpreterState
                        , evalContinuation, evalExec, initRefStore, initSchema, mkSQLiteEnv, setupEvalEnv)
import Pact.Gas (freeGasEnv)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.SQLite (SQLite, SQLiteConfig(..))
import Pact.Types.Capability (CapScope(..), CapSlot(..), SigCapability(..), capStack)
import Pact.Types.Command ( Command(..), CommandResult(..), PactResult(..), ParsedCode(..), Payload(..), ProcessedCommand(..)
                          , cmdToRequestKey, cmdPayload, pPayload, pSigners, verifyCommand)
import Pact.Types.ChainMeta (PublicMeta, pmSender)
import Pact.Types.Crypto (PPKScheme(..), PrivateKeyBS(..), PublicKeyBS(..))
import Pact.Types.Logger (Loggers(newLogger), alwaysLog)
import Pact.Types.RPC (PactRPC(..), ContMsg(..), ExecMsg(..))
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.Hash (Hash(..))
import Pact.Types.Persistence (ExecutionMode(..))
import Pact.Types.Runtime (Gas(..), GasEnv(..), GasLimit(..), EvalState, ModuleName(..), QualifiedName(..)
                          , catchesPactError, evalCapabilities, permissiveNamespacePolicy)
import Pact.Server.API (ApiV1API)
import Pact.Server.PactService (fullToHashLogCr)
import Pact.Types.API (ListenResponse, ListenerRequest, Poll, PollResponses, RequestKeys, SubmitBatch)
import Pact.Types.Server (throwCmdEx)

import Kadenamint.Common

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
  -> m a
applyCmd (DB pactDbEnv) stateF shouldRollback eval cmd = liftIO $ do
  case verifyCommand $ fmap T.encodeUtf8 cmd of
    f@ProcFail{} -> error (show f)
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

runCmd :: ((ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO EvalResult)
runCmd withExec withCont = \case
  Exec x -> withExec x
  Continuation c -> withCont c

runExec :: ((ExecMsg ParsedCode -> IO EvalResult) -> (ContMsg -> IO EvalResult) -> PactRPC ParsedCode -> IO EvalResult)
runExec withExec _ = \case
  Exec x -> withExec x
  _ -> throwCmdEx "local continuations not supported"

applyGenesisYaml :: MonadIO m => DB -> FilePath -> m EvalResult
applyGenesisYaml pactDbEnv fp = do
  (_, cmd) <- liftIO $ mkApiReq fp
  applyCmd pactDbEnv (initCapabilities [magic_COINBASE]) False runCmd cmd

mkExec' :: MonadIO m => Text -> Maybe Text -> m (Command Text)
mkExec' code sender = liftIO $ do
  senderKey <- traverse stockKey sender
  kps <- mkKeyPairs $ toList senderKey

  mkExec
    (T.unpack code)
    Aeson.Null
    (def & maybe id (pmSender .~) sender)
    kps
    Nothing
    Nothing

stockKey :: Text -> IO ApiKeyPair
stockKey s = do
    let Right (Y.Object o) = Y.decodeEither' stockKeyFile
        Just (Y.Object kp) = HM.lookup s o
        Just (Y.String pub) = HM.lookup "public" kp
        Just (Y.String priv) = HM.lookup "secret" kp
        mkKeyBS = fst . B16.decode . T.encodeUtf8
    return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) Nothing

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/coin-contract/keys.yaml")

initCapabilities :: [CapSlot SigCapability] -> EvalState -> EvalState
initCapabilities cs = set (evalCapabilities . capStack) cs

magic_COINBASE :: CapSlot SigCapability
magic_COINBASE = mkMagicCapSlot "COINBASE"

mkMagicCapSlot :: Text -> CapSlot SigCapability
mkMagicCapSlot c = CapSlot CapCallStack cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
    cap = SigCapability fqn []

sendHandler :: SubmitBatch -> Handler RequestKeys
sendHandler _ = todo Todo_ImplementEndpoint $ throwError err404

pollHandler :: Poll -> Handler PollResponses
pollHandler _ = todo Todo_ImplementEndpoint $ throwError err404

listenHandler :: ListenerRequest -> Handler ListenResponse
listenHandler _ = todo Todo_ImplementEndpoint $ throwError err404

localHandler ::  DB -> Command Text -> Handler (CommandResult Hash)
localHandler db cmd = do
  let maxGas (GasEnv (GasLimit g) _ _) = Gas $ fromIntegral g
  liftIO $ do
    er <- catchesPactError $ applyCmd db id True runExec cmd
    pure $ CommandResult
      { _crReqKey = cmdToRequestKey cmd
      , _crTxId = _erTxId <=< preview _Right $ er
      , _crResult = PactResult $ fmap lastOutput er
      , _crGas = either (const $ maxGas kadenamintGasEnv) _erGas er
      , _crLogs = fullToHashLogCr <$> evalLogs er
      , _crContinuation = noCont
      , _crMetaData = noMetadata
      }
  where
    lastOutput = todo Todo_NonemptyPactCode $ last . _erOutput
    evalLogs = assume Assumption_OnlyEvalLogs $ fmap _erLogs . preview _Right
    noCont = assume Assumption_NoLocalContinuations Nothing
    noMetadata = todo Todo_PlatformMetadata Nothing

infoHandler :: KadenamintVersion -> Handler NodeInfo
infoHandler v = pure $ NodeInfo
  { nodeVersion = v
  , nodeApiVersion = todo Todo_Versioning "0.0"
  , nodeChains = todo Todo_Naming ["0"]
  , nodeNumberOfChains = 1
  }


data KadenamintVersion
  = KadenamintVersion_Devnet_00
  deriving (Eq, Ord, Show, Generic)

instance ToJSON KadenamintVersion where
  toJSON KadenamintVersion_Devnet_00 = "kadenamint-devnet-00"

data NodeInfo = NodeInfo
  { nodeVersion :: KadenamintVersion
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  , nodeNumberOfChains :: !Int
  } deriving (Generic)

instance ToJSON NodeInfo

type PactAPI = ApiV1API
type ChainweaverAPI = "info" :> Get '[JSON] NodeInfo

kadenamintApi :: Proxy (ChainweaverAPI :<|> PactAPI)
kadenamintApi = Proxy

runApiServer :: DB -> IO ()
runApiServer db = do
  run port $ serve kadenamintApi $ chainweaverApiHandlers :<|> todo Todo_Versioning pactApiHandlers
  where
    port = assume Assumption_FreePort 8081
    chainweaverApiHandlers = infoHandler KadenamintVersion_Devnet_00
    pactApiHandlers = sendHandler
                 :<|> pollHandler
                 :<|> listenHandler
                 :<|> localHandler db
