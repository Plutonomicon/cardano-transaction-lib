module CTL.Examples.PlutusV2.ReferenceScripts
  ( main
  , example
  , contract
  ) where

import CTL.Contract.Prelude

import CTL.Contract.Address (scriptHashAddress)
import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import CTL.Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (ValidatorHash, validatorHash)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , mkTxUnspentOut
  )
import CTL.Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(SpendInput)
  , TxConstraints
  )
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Utxos (utxosAt)
import CTL.Contract.Value (lovelaceValueOf) as Value
import Data.BigInt (fromInt) as BigInt
import Data.Map (empty, toUnfoldable) as Map
import CTL.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import CTL.Examples.PlutusV2.AlwaysSucceeds (alwaysSucceedsScriptV2)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceScripts"
  validator <- alwaysSucceedsScriptV2
  let
    vhash :: ValidatorHash
    vhash = validatorHash validator

    scriptRef :: ScriptRef
    scriptRef = PlutusScriptRef (unwrap validator)

  logInfo' "Attempt to lock value"
  txId <- payWithScriptRefToAlwaysSucceeds vhash scriptRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId

payWithScriptRefToAlwaysSucceeds
  :: ValidatorHash -> ScriptRef -> Contract () TransactionHash
payWithScriptRefToAlwaysSucceeds vhash scriptRef = do
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScriptWithScriptRef vhash unitDatum DatumWitness
        scriptRef
        (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds :: ValidatorHash -> TransactionHash -> Contract () Unit
spendFromAlwaysSucceeds vhash txId = do
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress

  txInput /\ txOutput <-
    liftContractM "Could not find unspent output locked at script address"
      $ find hasTransactionId (Map.toUnfoldable utxos :: Array _)

  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
        (SpendInput $ mkTxUnspentOut txInput txOutput)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.unspentOutputs utxos

  spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId
