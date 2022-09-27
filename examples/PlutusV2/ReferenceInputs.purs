module Examples.PlutusV2.ReferenceInputs
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
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , MintingPolicyHash
  , PlutusScript
  , ValidatorHash
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , mkTxUnspentOut
  , plutusV2Script
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value (lovelaceValueOf) as Value
import Data.BigInt (fromInt) as BigInt
import Data.Map (Map)
import Data.Map (empty, toUnfoldable) as Map
import Examples.Helpers (buildBalanceSignAndSubmitTx, mkTokenName) as Helpers
import Examples.PlutusV2.AlwaysSucceeds (alwaysSucceedsScriptV2)

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
  publishTestFeedback true

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceInputs"
  validator <- alwaysSucceedsScriptV2
  plutusScript <- alwaysMintsPolicyV2
  tokenName <- Helpers.mkTokenName "TheToken"
  let
    vhash :: ValidatorHash
    vhash = validatorHash validator

    validatorRef :: ScriptRef
    validatorRef = PlutusScriptRef (unwrap validator)

    mpRef :: ScriptRef
    mpRef = PlutusScriptRef (unwrap validator)

  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId (unwrap validator) plutusScript
    tokenName

payToAlwaysSucceedsAndCreateScriptRefOutput
  :: ValidatorHash -> ScriptRef -> ScriptRef -> Contract () TransactionHash
payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef = do
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- ownStakePubKeyHash
  let
    value :: Value
    value = Value.lovelaceValueOf (BigInt.fromInt 2_000_000)

    createOutputWithScriptRef :: ScriptRef -> TxConstraints Unit Unit
    createOutputWithScriptRef scriptRef =
      mustPayToPubKeyStakeAddressWithScriptRef pkh skh scriptRef value

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash unitDatum DatumWitness value
        <> createOutputWithScriptRef validatorRef
        <> createOutputWithScriptRef mpRef

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  Helpers.buildBalanceSignAndSubmitTx lookups constraints

spendFromAlwaysSucceeds
  :: ValidatorHash
  -> TransactionHash
  -> PlutusScript
  -> PlutusScript
  -> TokenName
  -> Contract () Unit
spendFromAlwaysSucceeds vhash txId validator mp tokenName = do
  let scriptAddress = scriptHashAddress vhash
  ownAddress <- liftedM "Failed to get own address" getWalletAddress
  (utxos :: Array _) <- Map.toUnfoldable <$> utxosAt' ownAddress
  scriptAddressUtxos <- utxosAt' scriptAddress

  txInput /\ _ <-
    liftContractM "Could not find unspent output locked at script address"
      $ find hasTransactionId (Map.toUnfoldable scriptAddressUtxos :: Array _)

  refValidatorInput /\ refValidatorOutput <-
    liftContractM "Could not find unspent output containing ref validator"
      $ find (hasRefPlutusScript validator) utxos

  refMpInput /\ refMpOutput <-
    liftContractM "Could not find unspent output containing ref minting policy"
      $ find (hasRefPlutusScript mp) utxos

  let
    mph :: MintingPolicyHash
    mph = mintingPolicyHash (PlutusMintingPolicy mp)

    constraints :: TxConstraints Unit Unit
    constraints = mconcat
      [ Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
          (RefInput $ mkTxUnspentOut refValidatorInput refValidatorOutput)

      , Constraints.mustMintCurrencyUsingScriptRef mph tokenName one
          (RefInput $ mkTxUnspentOut refMpInput refMpOutput)
      ]

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.unspentOutputs scriptAddressUtxos

  spendTxId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values and minted tokens."
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput txInput /\ _) =
    txInput.transactionId == txId

  hasRefPlutusScript
    :: PlutusScript -> _ /\ TransactionOutputWithRefScript -> Boolean
  hasRefPlutusScript plutusScript (_ /\ txOutput) =
    (unwrap txOutput).scriptRef == Just (PlutusScriptRef plutusScript)

  utxosAt'
    :: Address
    -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
  utxosAt' = map (fromMaybe Map.empty) <<< utxosAt

mustPayToPubKeyStakeAddressWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints i o
mustPayToPubKeyStakeAddressWithScriptRef pkh Nothing =
  Constraints.mustPayToPubKeyWithScriptRef pkh
mustPayToPubKeyStakeAddressWithScriptRef pkh (Just skh) =
  Constraints.mustPayToPubKeyAddressWithScriptRef pkh skh

foreign import alwaysMintsV2 :: String

alwaysMintsPolicyV2 :: Contract () PlutusScript
alwaysMintsPolicyV2 =
  map plutusV2Script
    (textEnvelopeBytes alwaysMintsV2 PlutusScriptV2)
