module Ctl.Examples.PlutusV2.ReferenceInputsAndScripts
  ( contract
  , example
  , main
  , mintAlwaysMintsV2ToTheScript
  ) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , scriptHashAddress
  )
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , MintingPolicyHash
  , PlutusScript
  , Validator
  , ValidatorHash
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , mkTxUnspentOut
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Ctl.Examples.Helpers (mkTokenName) as Helpers
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints
  ( alwaysMintsPolicyScriptV2
  , alwaysMintsPolicyV2
  )
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Data.Array (head)
import Data.Map (toUnfoldable) as Map
import JS.BigInt (fromInt) as BigInt

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceInputsAndScripts"
  validator <- alwaysSucceedsScriptV2
  mintsScript <- alwaysMintsPolicyScriptV2
  tokenName <- Helpers.mkTokenName "TheToken"
  let
    vhash :: ValidatorHash
    vhash = validatorHash validator

    validatorRef :: ScriptRef
    validatorRef = PlutusScriptRef (unwrap validator)

    mpRef :: ScriptRef
    mpRef = PlutusScriptRef mintsScript

  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId (unwrap validator) mintsScript
    tokenName

payToAlwaysSucceedsAndCreateScriptRefOutput
  :: ValidatorHash -> ScriptRef -> ScriptRef -> Contract TransactionHash
payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef = do
  pkh <- liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes
  skh <- join <<< head <$> ownStakePubKeyHashes
  let
    value :: Value
    value = Value.lovelaceValueOf (BigInt.fromInt 2_000_000)

    createOutputWithScriptRef :: ScriptRef -> TxConstraints
    createOutputWithScriptRef scriptRef =
      mustPayToPubKeyStakeAddressWithScriptRef pkh skh scriptRef value

    constraints :: TxConstraints
    constraints =
      Constraints.mustPayToScript vhash unitDatum DatumWitness value
        <> createOutputWithScriptRef validatorRef
        <> createOutputWithScriptRef mpRef

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromAlwaysSucceeds
  :: ValidatorHash
  -> TransactionHash
  -> PlutusScript
  -> PlutusScript
  -> TokenName
  -> Contract Unit
spendFromAlwaysSucceeds vhash txId validator mp tokenName = do
  let scriptAddress = scriptHashAddress vhash Nothing
  ownAddress <- liftedM "Failed to get own address" $ head <$>
    getWalletAddresses
  (utxos :: Array _) <- Map.toUnfoldable <$> utxosAt ownAddress
  scriptAddressUtxos <- utxosAt scriptAddress

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

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
          (RefInput $ mkTxUnspentOut refValidatorInput refValidatorOutput)

      , Constraints.mustMintCurrencyUsingScriptRef mph tokenName one
          (RefInput $ mkTxUnspentOut refMpInput refMpOutput)
      ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs scriptAddressUtxos

  spendTxId <- submitTxFromConstraints lookups constraints
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

mustPayToPubKeyStakeAddressWithScriptRef
  :: forall (i :: Type) (o :: Type)
   . PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> ScriptRef
  -> Value
  -> TxConstraints
mustPayToPubKeyStakeAddressWithScriptRef pkh Nothing =
  Constraints.mustPayToPubKeyWithScriptRef pkh
mustPayToPubKeyStakeAddressWithScriptRef pkh (Just skh) =
  Constraints.mustPayToPubKeyAddressWithScriptRef pkh skh

mintAlwaysMintsV2ToTheScript
  :: TokenName -> Validator -> Int -> Contract Unit
mintAlwaysMintsV2ToTheScript tokenName validator sum = do
  mp <- alwaysMintsPolicyV2
  let cs = Value.scriptCurrencySymbol mp

  let
    vhash = validatorHash validator

    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue
          $ Value.singleton cs tokenName
          $ BigInt.fromInt sum
      , Constraints.mustPayToScript vhash unitDatum Constraints.DatumWitness
          $ Value.singleton cs tokenName
          $ BigInt.fromInt sum
      ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  txHash <- submitTxFromConstraints lookups constraints
  void $ awaitTxConfirmed txHash
