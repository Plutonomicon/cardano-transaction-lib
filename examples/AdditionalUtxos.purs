module Ctl.Examples.AdditionalUtxos
  ( contract
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( OutputWitness(PlutusScriptOutput)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(SpendOutput, Pay)
  )
import Cardano.Types
  ( OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , PlutusScript
  , ScriptHash
  , Transaction
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(ScriptHashCredential))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionUnspentOutput (fromUtxoMap)
import Contract.Address (mkAddress)
import Contract.BalanceTxConstraints
  ( BalancerConstraints
  , mustUseAdditionalUtxos
  )
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (Datum, PlutusData(Integer))
import Contract.Sync (withoutSync)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef)
  , awaitTxConfirmed
  , balanceTx
  , buildTx
  , createAdditionalUtxos
  , signTransaction
  , submit
  , withBalancedTx
  )
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Data.Map (difference, empty, filter) as Map
import JS.BigInt (fromInt) as BigInt
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example contractParams =
  launchAff_ $ runContract contractParams $ contract false

contract :: Boolean -> Contract Unit
contract testAdditionalUtxoOverlap = withoutSync do
  logInfo' "Running Examples.AdditionalUtxos"
  validator <- alwaysSucceedsScriptV2
  let vhash = PlutusScript.hash validator
  { unbalancedTx, datum } <- payToValidator vhash
  withBalancedTx unbalancedTx Map.empty mempty \balancedTx -> do
    balancedSignedTx <- signTransaction balancedTx
    txHash <- submit balancedSignedTx
    when testAdditionalUtxoOverlap $ awaitTxConfirmed txHash
    logInfo' "Successfully locked two outputs at the validator address."

    additionalUtxos <- createAdditionalUtxos balancedSignedTx
    spendFromValidator validator additionalUtxos datum

payToValidator
  :: ScriptHash
  -> Contract
       { unbalancedTx :: Transaction
       , datum :: Datum
       }
payToValidator vhash = do
  scriptRef <- liftEffect (NativeScriptRef <$> randomSampleOne arbitrary)
  let
    value :: Value
    value = Value.lovelaceValueOf $ BigNum.fromInt 2_000_000

    datum = Integer $ BigInt.fromInt 42
  scriptAddress <- mkAddress (PaymentCredential $ ScriptHashCredential vhash)
    Nothing

  tx <- buildTx
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: value
        , datum: Just $ OutputDatum $ datum
        , scriptRef: Nothing
        }
    , Pay $ TransactionOutput
        { address: scriptAddress
        , amount: value
        , datum: Just $ OutputDatum $ datum
        , scriptRef: Just scriptRef
        }
    ]

  pure { unbalancedTx: tx, datum }

spendFromValidator :: PlutusScript -> UtxoMap -> Datum -> Contract Unit
spendFromValidator validator additionalUtxos _datum = do
  addr <- mkAddress (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
    Nothing
  let
    scriptUtxos :: UtxoMap
    scriptUtxos =
      additionalUtxos # Map.filter \out ->
        (unwrap out).address == addr

    spendScriptOutputs = fromUtxoMap scriptUtxos <#> \output ->
      SpendOutput output $ Just $ PlutusScriptOutput (ScriptValue validator)
        RedeemerDatum.unit
        Nothing

    spendPubkeyOutputs =
      fromUtxoMap (Map.difference additionalUtxos scriptUtxos) <#> \output ->
        SpendOutput output Nothing

    plan = spendScriptOutputs <> spendPubkeyOutputs

    balancerConstraints :: BalancerConstraints
    balancerConstraints =
      mustUseAdditionalUtxos additionalUtxos

  unbalancedTx <- buildTx plan
  balancedTx <- balanceTx unbalancedTx additionalUtxos balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx

  awaitTxConfirmed txHash
  logInfo' "Successfully spent additional utxos from the validator address."
