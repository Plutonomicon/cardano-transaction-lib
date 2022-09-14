-- | Warning: This contract will permanently lock 7 Ada
-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a failing smart-contract transaction. It creates a
-- | transaction that pays two Ada to the `AlwaysFails` script address, and
-- | then attempts to spend the two Ada, failing and losing the collateral.
module CTL.Examples.Lose7Ada
  ( main
  , example
  , alwaysFailsScript
  , payToAlwaysFails
  , spendFromAlwaysFails
  ) where

import CTL.Contract.Prelude

import CTL.Contract.Address (scriptHashAddress)
import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad
  ( Contract
  , launchAff_
  , liftedE
  , runContract
  )
import CTL.Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (Validator, ValidatorHash, validatorHash)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import CTL.Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , plutusV1Script
  , submit
  )
import CTL.Contract.TxConstraints (TxConstraints)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Utxos (getWalletBalance, utxosAt)
import CTL.Contract.Value as Value
import CTL.Internal.BalanceTx.Collateral (minRequiredCollateral)
import Data.BigInt as BigInt
import Data.Foldable (fold)
import Data.Map as Map
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.AlwaysFails"
    validator <- alwaysFailsScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToAlwaysFails vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromAlwaysFails vhash validator txId
  publishTestFeedback true

payToAlwaysFails :: ValidatorHash -> Contract () TransactionHash
payToAlwaysFails vhash = do
  let
    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysFails
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromAlwaysFails vhash validator txId = do
  balanceBefore <- fold <$> getWalletBalance
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _) of
    Just txInput -> do
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
            <> Constraints.mustNotBeValid

      spendTxId <- buildBalanceSignAndSubmitTx lookups constraints
      logInfo' $ "Tx ID: " <> show spendTxId
      awaitTxConfirmed spendTxId
      logInfo' "Successfully spent locked values."

      balance <- fold <$> getWalletBalance
      let collateralLoss = Value.lovelaceValueOf (-minRequiredCollateral)
      balance `shouldEqual` (balanceBefore <> collateralLoss)

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

foreign import alwaysFails :: String

alwaysFailsScript :: Contract () Validator
alwaysFailsScript = wrap <<< plutusV1Script <$> textEnvelopeBytes
  alwaysFails
  PlutusScriptV1
