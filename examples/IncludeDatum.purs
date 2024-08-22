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

import Cardano.Transaction.Builder
  ( DatumWitness(DatumValue)
  , OutputWitness(PlutusScriptOutput)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(SpendOutput, Pay)
  )
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatumHash)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (PlutusData(Integer))
import Contract.Scripts (Validator, ValidatorHash, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , lookupTxHash
  , submitTxFromBuildPlan
  )
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt as BigInt

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
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
payToIncludeDatum vhash = do
  address <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatumHash $ hashPlutusData datum
        , scriptRef: Nothing
        }
    ]

spendFromIncludeDatum
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromIncludeDatum vhash validator txId = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  utxos <- utxosAt scriptAddress
  utxo <- liftContractM "no locked output at address"
    (head (lookupTxHash txId utxos))
  spendTx <- submitTxFromBuildPlan (toUtxoMap [ utxo ])
    mempty
    [ SpendOutput
        utxo
        ( Just $ PlutusScriptOutput (ScriptValue validator) RedeemerDatum.unit
            $ Just
            $ DatumValue datum
        )
    ]
  awaitTxConfirmed $ Transaction.hash spendTx
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
