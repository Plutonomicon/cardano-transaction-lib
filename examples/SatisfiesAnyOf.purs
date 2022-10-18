-- | This module creates a transaction
-- | that pays 2 Ada to the `IncludeDatum` script address
-- | and then spends the script Utxo. The script only checks
-- | that the value of the datum is equal to 42.
module Ctl.Examples.SatisfiesAnyOf
  ( example
  , main
  , payToSatisfiesAnyOf
  , spendFromSatisfiesAnyOf
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData(Integer)
  , unitDatum
  , unitRedeemer
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction (TransactionHash, awaitTxConfirmed, lookupTxHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript) as AlwaysSucceeds
import Ctl.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import Ctl.Internal.Hashing (datumHash) as Hashing
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (_input)
import Ctl.Internal.Types.ByteArray (hexToByteArrayUnsafe)
import Ctl.Internal.Types.Transaction (DataHash(DataHash))
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Lens (view)
import Data.Map as Map

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.SatisfiesAnyOf"
    validator <- AlwaysSucceeds.alwaysSucceedsScript
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToSatisfiesAnyOf vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromSatisfiesAnyOf vhash validator txId
  publishTestFeedback true

wrongDatum :: Datum
wrongDatum = Datum $ Integer $ BigInt.fromInt 42

payToSatisfiesAnyOf :: ValidatorHash -> Contract () TransactionHash
payToSatisfiesAnyOf vhash = do
  let
    wrongDatumHash = fromMaybe (DataHash $ hexToByteArrayUnsafe "bb") $
      Hashing.datumHash wrongDatum
    correctDatumHash = fromMaybe (DataHash $ hexToByteArrayUnsafe "bb") $
      Hashing.datumHash unitDatum

    payToConstraint :: TxConstraints Unit Unit
    payToConstraint =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000

    payToConstraintTooMuch :: TxConstraints Unit Unit
    payToConstraintTooMuch =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ BigInt.fromInt 2_000_000_000

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSatisfyAnyOf
      [ payToConstraintTooMuch <> Constraints.mustHashDatum correctDatumHash unitDatum
      , payToConstraint <> Constraints.mustHashDatum correctDatumHash unitDatum
      ]

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromSatisfiesAnyOf
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromSatisfiesAnyOf vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  utxos <- fromMaybe Map.empty <$> utxosAt scriptAddress
  case view _input <$> head (lookupTxHash txId utxos) of
    Just txInput ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints = Constraints.mustSpendScriptOutput txInput unitRedeemer
      in
        do
          spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
          awaitTxConfirmed spendTxId
          logInfo' "Successfully spent locked values."
    _ -> logInfo' "no locked output at address"
