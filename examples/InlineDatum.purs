-- | This module demonstrates creating a UTxO whose datum is inline via the
-- | `Contract` interface. The `checkDatumIsInlineScript` only validates if the
-- | scripts own input was supplied with an inline datum matching the redeemer.
module Examples.InlineDatum
  ( main
  , example
  , checkDatumIsInlineScript
  , payToCheckDatumIsInline
  , payToCheckDatumIsInlineWrong
  , readFromCheckDatumIsInline
  , spendFromCheckDatumIsInline
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftedE
  , runContract
  )
import Contract.PlutusData
  ( PlutusData(Integer)
  , Datum(Datum)
  , Redeemer(Redeemer)
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , OutputDatum(OutputDatum)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , submit
  , plutusV2Script
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.InlineDatum"
    validator <- checkDatumIsInlineScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToCheckDatumIsInline vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromCheckDatumIsInline vhash validator txId
  publishTestFeedback true

plutusData :: PlutusData
plutusData = Integer $ BigInt.fromInt 31415927

payToCheckDatumIsInline :: ValidatorHash -> Contract () TransactionHash
payToCheckDatumIsInline vhash = do
  let
    datum :: Datum
    datum = Datum plutusData

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash datum
        Constraints.DatumInline
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints

spendFromCheckDatumIsInline
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromCheckDatumIsInline vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _) of
    Just txInput -> do
      let
        redeemer :: Redeemer
        redeemer = Redeemer plutusData

        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput redeemer

      spendTxId <- buildBalanceSignAndSubmitTx lookups constraints
      awaitTxConfirmed spendTxId
      logInfo' "Successfully spent locked values."

    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at: "
        <> show scriptAddress
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

payToCheckDatumIsInlineWrong :: ValidatorHash -> Contract () TransactionHash
payToCheckDatumIsInlineWrong vhash = do
  let
    datum :: Datum
    datum = Datum plutusData

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash datum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints

readFromCheckDatumIsInline
  :: ValidatorHash
  -> TransactionHash
  -> Contract () Unit
readFromCheckDatumIsInline vhash txId = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  case snd <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _) of
    Just (TransactionOutput { datum }) -> do
      datum `shouldEqual` OutputDatum (Datum plutusData)
      logInfo' "Successfully read inline datum."

    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at: "
        <> show scriptAddress
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

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

foreign import checkDatumIsInline :: String

checkDatumIsInlineScript :: Contract () Validator
checkDatumIsInlineScript = wrap <<< plutusV2Script <$> textEnvelopeBytes
  checkDatumIsInline
  PlutusScriptV2
