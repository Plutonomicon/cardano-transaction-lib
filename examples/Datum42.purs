-- | This module creates a transaction
-- | that pays 2 Ada to the `Datum42` script address
-- | and then spends the script Utxo. The script only checks
-- | that the value of the datum is equal to 42.
module Ctl.Examples.Datum42
  ( example
  , datum42Script
  , main
  , payToDatum42
  , spendFromDatum42
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedE, runContract)
import Contract.PlutusData (Datum(Datum), PlutusData(Integer), unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceAndSignTxE
  , lookupTxHash
  , plutusV1Script
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (_input)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Lens (view)
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.Datum42"
    validator <- datum42Script
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToDatum42 vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromDatum42 vhash validator txId
  publishTestFeedback true

datum :: Datum
datum = Datum $ Integer $ BigInt.fromInt 42

payToDatum42 :: ValidatorHash -> Contract () TransactionHash
payToDatum42 vhash = do
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      ( Constraints.mustPayToScript vhash datum Constraints.DatumWitness
          $ Value.lovelaceValueOf
          $ BigInt.fromInt 2_000_000
      )
        <> Constraints.mustIncludeDatum datum

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
  buildBalanceSignAndSubmitTx lookups constraints

spendFromDatum42
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromDatum42 vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress
  case view _input <$> head (lookupTxHash txId utxos) of
    Just txInput ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
            <> Constraints.mustIncludeDatum datum
      in
        do
          spendTxId <- buildBalanceSignAndSubmitTx lookups constraints
          awaitTxConfirmed spendTxId
          logInfo' "Successfully spent locked values."
    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at: "
        <> show scriptAddress

buildBalanceSignAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ balanceAndSignTxE ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

foreign import datum42 :: String

-- | checks if the datum equals 42
datum42Script :: Contract () Validator
datum42Script = wrap <<< plutusV1Script <$> textEnvelopeBytes
  datum42
  PlutusScriptV1
