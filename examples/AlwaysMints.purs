-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module Ctl.Examples.AlwaysMints
  ( alwaysMintsPolicy
  , alwaysMintsPolicyMaybe
  , contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(NativeScriptCredential, PlutusScriptCredential)
  , ScriptWitness(ScriptValue, ScriptReference)
  , TransactionBuilderStep
      ( SpendOutput
      , Pay
      , MintAsset
      , RegisterStake
      , IssueCertificate
      , WithdrawStake
      , RequireSignature
      , RegisterPool
      , RetirePool
      , IncludeDatum
      , SetTTL
      , SetValidityStartInterval
      , SetIsValid
      )
  )
import Cardano.Types (PlutusScript)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Transaction as Transaction
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (unitRedeemer)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.AlwaysMints"
  mintingPolicy <- alwaysMintsPolicy
  let scriptHash = PlutusScript.hash mintingPolicy
  tokenName <- Helpers.mkAssetName "TheToken"
  awaitTxConfirmed <<< Transaction.hash =<<
    submitTxFromBuildPlan Map.empty mempty
      [ MintAsset
          scriptHash
          tokenName
          (Int.fromInt 100)
          (PlutusScriptCredential (ScriptValue mintingPolicy) unitRedeemer)
      ]
  logInfo' "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

foreign import alwaysMints :: String

alwaysMintsPolicyMaybe :: Maybe PlutusScript
alwaysMintsPolicyMaybe =
  plutusScriptFromEnvelope =<< decodeTextEnvelope alwaysMints

alwaysMintsPolicy :: Contract PlutusScript
alwaysMintsPolicy =
  liftContractM "Error decoding alwaysMintsPolicy" alwaysMintsPolicyMaybe
