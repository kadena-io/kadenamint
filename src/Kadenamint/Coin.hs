{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Kadenamint.Coin where

import Control.Lens                                     ((&))
import Control.Monad.Except                             (ExceptT(..), runExceptT)
import Control.Monad.IO.Class                           (MonadIO(..))
import Data.Decimal                                     (Decimal, roundTo)
import Data.Default                                     (def)
import Data.Functor                                     (void)
import Data.String.Here.Uninterpolated                  (here)
import Data.Text                                        (Text)
import qualified Data.Text as T

import Pact.ApiReq                                      (mkApiReq)
import Pact.Interpreter                                 (EvalResult)
import Pact.Types.Capability                            (CapScope(CapCallStack), CapSlot(..), SigCapability(..))
import Pact.Types.Exp                                   (Literal(LString, LDecimal))
import Pact.Types.Info                                  (Info(..))
import Pact.Types.Names                                 (ModuleName(..), QualifiedName(..))
import Pact.Types.PactError                             (PactError)
import Pact.Types.PactValue                             (PactValue(PLiteral))

import Kadenamint.Common
import Kadenamint.Pact

applyCoinGenesis :: MonadIO m => DB -> (Text -> m ()) -> m (Either PactError ())
applyCoinGenesis pactDbEnv logger = runExceptT $ do
  let eval msg yaml = void $ ExceptT $
        applyGenesisYaml pactDbEnv yaml <* logger msg

  eval "Initialized fungible interface" "pact/coin-contract/v2/load-fungible-asset-v2.yaml"
  eval "Initialized coin contract"      "pact/coin-contract/v2/load-coin-contract-v2.yaml"
  eval "Initialized coin accounts"      "pact/genesis/devnet/grants.yaml"

applyGenesisYaml :: MonadIO m => DB -> FilePath -> m (Either PactError EvalResult)
applyGenesisYaml pactDbEnv fp = do
  (_, cmd) <- liftIO $ mkApiReq fp
  applyCmd pactDbEnv (initCapabilities [magic_COINBASE]) False runCmd cmd

showBalances :: Text
showBalances = [here|
  { "sender00" : (coin.get-balance 'sender00)
  , "sender01" : (coin.get-balance 'sender01)
  , "sender02" : (coin.get-balance 'sender02)
  }
|]

transfer :: Text -> Text -> Decimal -> Text
transfer from to amount = T.intercalate " "
  [ "(coin.transfer"
  , "'" <> from
  , "'" <> to
  , tshow $ forceDecimalPoint amount
  , ")"
  ]
  where
    forceDecimalPoint :: Decimal -> Decimal
    forceDecimalPoint d = d &
      if d == roundTo 0 d
      then roundTo 1
      else id

mkTransferCapability :: Text -> Text -> Decimal -> SigCapability
mkTransferCapability from to amount = SigCapability qn
    [ PLiteral $ LString from
    , PLiteral $ LString to
    , PLiteral $ LDecimal amount
    ]
  where
    qn = QualifiedName cm "TRANSFER" (Info Nothing)
    cm = ModuleName "coin" Nothing

magic_COINBASE :: CapSlot SigCapability
magic_COINBASE = mkMagicCapSlot "COINBASE"

mkMagicCapSlot :: Text -> CapSlot SigCapability
mkMagicCapSlot c = CapSlot CapCallStack cap []
  where
    mn = ModuleName "coin" Nothing
    fqn = QualifiedName mn c def
    cap = SigCapability fqn []
