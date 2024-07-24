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

import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(ScriptHashCredential))
import Cardano.Types.PlutusData (unit) as PlutusData
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf, minus) as Value
import Contract.Wallet (getWalletBalance)
import Control.Monad.Error.Class (liftMaybe)
import Data.Foldable (fold)
import Data.Functor ((<$>))
import Data.Map as Map
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "nami" { cip95: false }
  }

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
    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript vhash PlutusData.unit
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigNum.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromAlwaysFails
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysFails vhash validator txId = do
  balanceBefore <- unsafePartial $ fold <$> getWalletBalance
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
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
    lookups :: Lookups.ScriptLookups
    lookups = mconcat
      [ Lookups.validator validator
      , Lookups.unspentOutputs utxos
      , Lookups.datum PlutusData.unit
      ]

    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutput txInput unitRedeemer
        <> Constraints.mustNotBeValid

  spendTxId <- submitTxFromConstraints lookups constraints
  logInfo' $ "Tx ID: " <> show spendTxId
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

  balance <- unsafePartial $ fold <$> getWalletBalance
  let collateralLoss = Value.lovelaceValueOf $ BigNum.fromInt (5_000_000)
  Just balance `shouldEqual` (Value.minus balanceBefore collateralLoss)

  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId

alwaysFailsScript :: Contract Validator
alwaysFailsScript = do
  liftMaybe (error "Error decoding alwaysFails") do
    envelope <- decodeTextEnvelope alwaysFails
    plutusScriptFromEnvelope envelope

alwaysFails :: String
alwaysFails =
  """
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "581e581c01000033223232222350040071235002353003001498498480048005"
}
"""
