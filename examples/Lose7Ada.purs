-- | Warning: This contract will permanently lock 7 Ada
-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a failing smart-contract transaction. It creates a
-- | transaction that pays two Ada to the `AlwaysFails` script address, and
-- | then attempts to spend the two Ada, failing and losing the collateral.
module Ctl.Examples.Lose7Ada
  ( main
  , example
  , alwaysFailsScript
  , payToAlwaysFails
  , spendFromAlwaysFails
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), ValidatorHash, validatorHash)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet (getWalletBalance)
import Control.Monad.Error.Class (liftMaybe)
import Data.BigInt as BigInt
import Data.Foldable (fold)
import Data.Functor ((<$>))
import Data.Map as Map
import Effect.Exception (error)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
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

payToAlwaysFails :: ValidatorHash -> Contract TransactionHash
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

  submitTxFromConstraints lookups constraints

spendFromAlwaysFails
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysFails vhash validator txId = do
  balanceBefore <- fold <$> getWalletBalance
  let scriptAddress = scriptHashAddress vhash Nothing
  utxos <- utxosAt scriptAddress
  txInput <- liftM
    ( error
        ( "The id "
            <> show txId
            <> " does not have output locked at: "
            <> show scriptAddress
        )
    )
    (fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _))
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutput txInput unitRedeemer
        <> Constraints.mustNotBeValid

  spendTxId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show spendTxId
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

  balance <- fold <$> getWalletBalance
  let collateralLoss = Value.lovelaceValueOf $ BigInt.fromInt (-5_000_000)
  balance `shouldEqual` (balanceBefore <> collateralLoss)

  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

foreign import alwaysFails :: String

alwaysFailsScript :: Contract Validator
alwaysFailsScript =
  liftMaybe (error "Error decoding alwaysFails") do
    envelope <- decodeTextEnvelope alwaysFails
    Validator <$> plutusScriptV1FromEnvelope envelope
