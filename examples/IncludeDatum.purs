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

import Cardano.Types (Credential(ScriptHashCredential))
import Cardano.Types.BigNum as BigNum
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (PlutusData(Integer), unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , _input
  , awaitTxConfirmed
  , lookupTxHash
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Lens (view)
import Effect.Exception (error)
import JS.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
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

datum :: PlutusData
datum = Integer $ BigInt.fromInt 42

payToIncludeDatum :: ValidatorHash -> Contract TransactionHash
payToIncludeDatum vhash =
  let
    constraints :: TxConstraints
    constraints =
      ( Constraints.mustPayToScript vhash datum Constraints.DatumWitness
          $ Value.lovelaceValueOf
          $ BigNum.fromInt 2_000_000
      )
        <> Constraints.mustIncludeDatum datum

    lookups :: Lookups.ScriptLookups
    lookups = mempty
  in
    submitTxFromConstraints lookups constraints

spendFromIncludeDatum
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromIncludeDatum vhash validator txId = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  txInput <- liftContractM "no locked output at address"
    (view _input <$> head (lookupTxHash txId utxos))
  let
    lookups :: Lookups.ScriptLookups
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutput txInput unitRedeemer
        <> Constraints.mustIncludeDatum datum
  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

-- | checks if the datum equals 42
only42Script :: Contract Validator
only42Script = do
  liftMaybe (error "Error decoding includeDatum") do
    envelope <- decodeTextEnvelope includeDatum
    plutusScriptFromEnvelope envelope

includeDatum :: String
includeDatum =
  """
{
    "type": "PlutusScriptV1",
    "description": "include-datum",
    "cborHex": "55540100002225333573466e1cdd6801a40a82930b01"
}
"""
