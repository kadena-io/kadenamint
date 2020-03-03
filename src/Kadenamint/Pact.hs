{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Kadenamint.Pact where

import Control.Lens (set, (&), (^.), (.~))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (Default (..))
import Data.FileEmbed (embedFile)
import Data.Foldable (Foldable(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y

import Pact.ApiReq (ApiKeyPair(..), mkApiReq, mkExec, mkKeyPairs)
import Pact.Interpreter (EvalResult(..), MsgData(..), PactDbEnv, defaultInterpreterState
                        , evalContinuation, evalExec, initRefStore, initSchema, mkSQLiteEnv, setupEvalEnv)
import Pact.Gas (freeGasEnv)
import Pact.PersistPactDb (DbEnv)
import Pact.Persist.SQLite (SQLite, SQLiteConfig(..))
import Pact.Types.Capability (SigCapability(..))
import Pact.Types.Command (Command(..), ParsedCode(..), Payload(..), ProcessedCommand(..), cmdPayload, pPayload, pSigners, verifyCommand)
import Pact.Types.ChainMeta (PublicMeta, pmSender)
import Pact.Types.Crypto (PPKScheme(..), PrivateKeyBS(..), PublicKeyBS(..))
import Pact.Types.Logger (Loggers(newLogger), alwaysLog)
import Pact.Types.RPC (PactRPC(..), ExecMsg(..))
import Pact.Types.SPV (noSPVSupport)
import Pact.Types.Hash (Hash(..))
import Pact.Types.Persistence (ExecutionMode(..))
import Pact.Types.Capability (CapScope(..), CapSlot(..), capStack)
import Pact.Types.Runtime (EvalState, ModuleName(..), QualifiedName(..), evalCapabilities, permissiveNamespacePolicy)

newtype DB = DB { unDB :: PactDbEnv (DbEnv SQLite) }

initDb :: MonadIO m => FilePath -> m DB
initDb path = liftIO $ do
  pactDbEnv <- mkSQLiteEnv (newLogger alwaysLog "") True (SQLiteConfig path []) alwaysLog
  initSchema pactDbEnv
  pure $ DB pactDbEnv

execCmd :: MonadIO m => DB -> (EvalState -> EvalState) -> Bool -> Command Text -> m EvalResult
execCmd (DB pactDbEnv) stateF shouldRollback cmd = liftIO $ do
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
          freeGasEnv
          permissiveNamespacePolicy
          noSPVSupport
          def
          def

      case p ^. pPayload of
        Exec (ExecMsg code execData) -> evalExec interpreter (setupEvalEnv' (Just execData)) code
        Continuation cont -> evalContinuation interpreter (setupEvalEnv' Nothing) cont

execYaml :: MonadIO m => DB -> FilePath -> m EvalResult
execYaml pactDbEnv fp = do
  (_, exec) <- liftIO $ mkApiReq fp
  execCmd pactDbEnv (initCapabilities [magic_COINBASE]) False exec

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
