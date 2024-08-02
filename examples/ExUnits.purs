module Ctl.Examples.ExUnits where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( OutputWitness(PlutusScriptOutput)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(Pay, SpendOutput)
  )
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData as PlutusData
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
import Contract.Credential (Credential(PubKeyHashCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (RedeemerDatum(RedeemerDatum), toData)
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
import Contract.Wallet (ownStakePubKeyHashes)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.ExUnits"
  validator <- exUnitsScript
  let vhash = validatorHash validator
  logInfo' "Attempt to lock value"
  txId <- payToExUnits vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromExUnits (BigInt.fromInt 1000) vhash validator txId

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

payToExUnits :: ValidatorHash -> Contract TransactionHash
payToExUnits vhash = do
  address <- mkAddress
    (PaymentCredential $ ScriptHashCredential vhash)
    Nothing
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: address
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatum PlutusData.unit
        , scriptRef: Nothing
        }
    ]

-- | ExUnits script loops a given number of iterations provided as redeemer.
spendFromExUnits
  :: BigInt
  -> ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract Unit
spendFromExUnits iters vhash validator txId = do
  -- Use own stake credential if available
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <-
    mkAddress (wrap $ ScriptHashCredential vhash)
      (wrap <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  utxos <- utxosAt scriptAddress
  utxo <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      (head (lookupTxHash txId utxos))

  spendTx <- submitTxFromBuildPlan (toUtxoMap [ utxo ])
    mempty
    [ SpendOutput
        utxo
        $ Just
        $ PlutusScriptOutput (ScriptValue validator)
            (RedeemerDatum $ toData iters)
            Nothing
    ]
  awaitTxConfirmed $ Transaction.hash spendTx
  logInfo' "Successfully spent locked values."

exUnitsScript :: Contract Validator
exUnitsScript = do
  liftMaybe (error "Error decoding exUnits") do
    envelope <- decodeTextEnvelope exUnits
    plutusScriptFromEnvelope envelope

foreign import exUnits :: String
