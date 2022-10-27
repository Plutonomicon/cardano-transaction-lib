-- | This module creates a transaction
-- | that pays 2 Ada to the `IncludeDatum` script address
-- | and then spends the script Utxo. The script only checks
-- | that the value of the datum is equal to 42.
module Ctl.Examples.IncludeDatum
  ( example
  , only42Script
  , main
  , payToIncludeDatum
  , spendFromIncludeDatum
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (Datum(Datum), PlutusData(Integer), unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(..), ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( liftEitherTextEnvelopeDecodeError
  , plutusScriptV1FromEnvelope
  )
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , lookupTxHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
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
    logInfo' "Running Examples.IncludeDatum"
    validator <- only42Script
    let vhash = validatorHash validator
    logInfo' "Attempt to lock value"
    txId <- payToIncludeDatum vhash
    awaitTxConfirmed txId
    logInfo' "Tx submitted successfully, Try to spend locked values"
    spendFromIncludeDatum vhash validator txId
  publishTestFeedback true

datum :: Datum
datum = Datum $ Integer $ BigInt.fromInt 42

payToIncludeDatum :: ValidatorHash -> Contract () TransactionHash
payToIncludeDatum vhash =
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
  in
    Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromIncludeDatum
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
spendFromIncludeDatum vhash validator txId = do
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
          spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
          awaitTxConfirmed spendTxId
          logInfo' "Successfully spent locked values."
    _ -> logInfo' "no locked output at address"

foreign import includeDatum :: String

-- | checks if the datum equals 42
only42Script :: Contract () Validator
only42Script =
  liftEitherTextEnvelopeDecodeError
    $ Validator
    <$> plutusScriptV1FromEnvelope includeDatum