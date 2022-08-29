module Examples.ReferenceScripts
  ( main
  , example
  , contract
  , alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , mkTxUnspentOut
  , plutusV2Script
  )
import Contract.TxConstraints
  ( InputWithScriptRef(SpendableInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value (lovelaceValueOf) as Value
import Data.BigInt (fromInt) as BigInt
import Data.Map (empty, toUnfoldable) as Map
import Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.ReferenceScripts"
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
      Constraints.mustPayWithScriptRefToScript vhash unitDatum scriptRef
        (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds :: ValidatorHash -> TransactionHash -> Contract () Unit
spendFromAlwaysSucceeds vhash txId = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress

  txInput /\ txOutput <-
    liftContractM "Could not find unspent output locked at script address"
      $ find hasTransactionId (Map.toUnfoldable utxos :: Array _)

  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
        (SpendableInput $ mkTxUnspentOut txInput txOutput)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.unspentOutputs utxos

  spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

foreign import alwaysSucceedsV2 :: String

alwaysSucceedsScriptV2 :: Contract () Validator
alwaysSucceedsScriptV2 =
  map (wrap <<< plutusV2Script)
    (textEnvelopeBytes alwaysSucceedsV2 PlutusScriptV2)

