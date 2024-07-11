-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address.
module Ctl.Examples.AlwaysSucceeds
  ( alwaysSucceedsScript
  , contract
  , example
  , main
  , payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( DatumWitness(DatumValue, DatumReference)
  , OutputWitness(NativeScriptOutput, PlutusScriptOutput)
  , ScriptWitness(ScriptValue, ScriptReference)
  , TransactionBuilderStep(SpendOutput, Pay)
  )
import Cardano.Types
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , PlutusScript
  , ScriptHash
  , StakeCredential(StakeCredential)
  , TransactionHash
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as Script
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (unitRedeemer)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( awaitTxConfirmed
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

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.AlwaysSucceeds"
  validator <- alwaysSucceedsScript
  let vhash = Script.hash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

payToAlwaysSucceeds :: ScriptHash -> Contract TransactionHash
payToAlwaysSucceeds vhash = do
  -- Send to own stake credential. This is used to test mustPayToScriptAddress.
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress (PaymentCredential $ ScriptHashCredential vhash)
    (StakeCredential <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
        , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
        , scriptRef: Nothing
        }
    ]

spendFromAlwaysSucceeds
  :: ScriptHash
  -> PlutusScript
  -> TransactionHash
  -> Contract Unit
spendFromAlwaysSucceeds vhash validator txId = do
  -- Use own stake credential if available
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress
    (wrap $ ScriptHashCredential vhash)
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
      $ head (lookupTxHash txId utxos)
  spendTx <- submitTxFromBuildPlan (Map.union utxos $ toUtxoMap [ utxo ])
    mempty
    [ SpendOutput
        utxo
        ( Just $ PlutusScriptOutput (ScriptValue validator) unitRedeemer $ Just
            $ DatumValue
            $ PlutusData.unit
        )
    ]
  awaitTxConfirmed $ Transaction.hash spendTx
  logInfo' "Successfully spent locked values."

alwaysSucceedsScript :: Contract PlutusScript
alwaysSucceedsScript = do
  liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope alwaysSucceeds
    plutusScriptFromEnvelope envelope

foreign import alwaysSucceeds :: String
