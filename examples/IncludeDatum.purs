-- | This module creates a transaction
-- | that pays 2 Ada to the `IncludeDatum` script address
-- | and then spends the script Utxo. The script only checks
-- | that the value of the datum is equal to 42.
module Ctl.Examples.IncludeDatum
  ( contract
  , example
  , only42Script
  , main
  , payToIncludeDatum
  , spendFromIncludeDatum
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder (TransactionBuilderStep(Pay))
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatumHash)
  , PlutusScript
  , ScriptHash
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (PlutusData(Integer))
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , lookupTxHash
  , submitTxFromBlueprint
  , submitTxFromConstraints
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustIncludeDatum, mustSpendScriptOutput) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Effect.Exception (error)
import JS.BigInt as BigInt

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.IncludeDatum"
  validator <- only42Script
  let vhash = PlutusScript.hash validator
  logInfo' "Attempt to lock value"
  txId <- payToIncludeDatum vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromIncludeDatum vhash validator txId

datum :: PlutusData
datum = Integer $ BigInt.fromInt 42

payToIncludeDatum :: ScriptHash -> Contract TransactionHash
payToIncludeDatum vhash = do
  address <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  _.txHash <$> submitTxFromBlueprint
    { buildSteps:
        [ Pay $ TransactionOutput
            { address
            , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
            , datum: Just $ OutputDatumHash $ hashPlutusData datum
            , scriptRef: Nothing
            }
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }

spendFromIncludeDatum
  :: ScriptHash
  -> PlutusScript
  -> TransactionHash
  -> Contract Unit
spendFromIncludeDatum vhash validator txId = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  txInput <- liftContractM "no locked output at address"
    (_.input <<< unwrap <$> head (lookupTxHash txId utxos))
  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutput txInput RedeemerDatum.unit
        <> Constraints.mustIncludeDatum datum

    lookups :: ScriptLookups
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

  spendTxHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxHash
  logInfo' "Successfully spent locked values."

-- | checks if the datum equals 42
only42Script :: Contract PlutusScript
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
