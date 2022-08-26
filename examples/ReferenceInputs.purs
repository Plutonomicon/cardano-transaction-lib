module Examples.ReferenceInputs
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , scriptHashAddress
  )
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ScriptHash, ValidatorHash, validatorHash)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput
  , awaitTxConfirmed
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map)
import Data.Map (empty, toUnfoldable) as Map
import Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import Examples.ReferenceScripts (alwaysSucceedsScriptV2)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.ReferenceInputs"
  validator <- alwaysSucceedsScriptV2
  let
    vhash :: ValidatorHash
    vhash = validatorHash validator

    scriptRef :: ScriptRef
    scriptRef = PlutusScriptRef (unwrap validator)

  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceedsAndCreateScriptRefOutput vhash scriptRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId (unwrap vhash)

payToAlwaysSucceedsAndCreateScriptRefOutput
  :: ValidatorHash -> ScriptRef -> Contract () TransactionHash
payToAlwaysSucceedsAndCreateScriptRefOutput vhash scriptRef = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  let
    value :: Value
    value = Value.lovelaceValueOf (BigInt.fromInt 2_000_000)

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash unitDatum value
        <> mustPayWithScriptRefToPubKeyStakeAddress pkh skh scriptRef value

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds
  :: ValidatorHash -> TransactionHash -> ScriptHash -> Contract () Unit
spendFromAlwaysSucceeds vhash txId refScriptHash = do
  let scriptAddress = scriptHashAddress vhash
  ownAddress <- liftedM "Failed to get own address" getWalletAddress
  (utxos :: Array _) <- Map.toUnfoldable <$> utxosAt' ownAddress
  scriptAddressUtxos <- utxosAt' scriptAddress
  let
    mTxInput :: Maybe TransactionInput
    mTxInput = fst <$>
      find hasTransactionId (Map.toUnfoldable scriptAddressUtxos :: Array _)

    mRefScriptUnspentOut :: Maybe TransactionUnspentOutput
    mRefScriptUnspentOut =
      find (hasAlwaysSucceedsScriptRef refScriptHash) utxos
        <#> \(input /\ output) -> mkTxUnspentOut input output

  case mTxInput /\ mRefScriptUnspentOut of
    Just txInput /\ Just refScriptUnspentOut ->
      let
        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
            (RefInput refScriptUnspentOut)

        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.unspentOutputs scriptAddressUtxos
      in
        do
          spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
          awaitTxConfirmed spendTxId
          logInfo' "Successfully spent locked values."

    _ ->
      logInfo' $ "txInput: "
        <> show mTxInput
        <> ", refInput: "
        <> show mRefScriptUnspentOut
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput txInput /\ _) =
    txInput.transactionId == txId

  hasAlwaysSucceedsScriptRef :: ScriptHash -> _ /\ TransactionOutput -> Boolean
  hasAlwaysSucceedsScriptRef scriptHash (_ /\ TransactionOutput txOutput) =
    txOutput.referenceScript == Just scriptHash

  utxosAt' :: Address -> Contract () (Map TransactionInput TransactionOutput)
  utxosAt' = map (unwrap <<< fromMaybe (UtxoM Map.empty)) <<< utxosAt

mustPayWithScriptRefToPubKeyStakeAddress
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayWithScriptRefToPubKeyStakeAddress pkh Nothing =
  Constraints.mustPayWithScriptRefToPubKey pkh
mustPayWithScriptRefToPubKeyStakeAddress pkh (Just skh) =
  Constraints.mustPayWithScriptRefToPubKeyAddress pkh skh

