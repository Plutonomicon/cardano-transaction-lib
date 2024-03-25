module Ctl.Examples.PlutusV2.ReferenceScripts
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Credential (Credential(PubKeyCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (unitDatum, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
  , mkTxUnspentOut
  , submitTxFromConstraints
  )
import Contract.TxConstraints
  ( DatumPresence(DatumWitness)
  , InputWithScriptRef(SpendInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (ownStakePubKeyHashes)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Data.Array (head)
import Data.Map (toUnfoldable) as Map
import JS.BigInt (fromInt) as BigInt

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

-- NOTE: If you are looking for an example of the most common case of 
-- using reference scripts by referencing an output and not spending it,
-- you likely need to look at the example in `ReferenceInputsAndScripts.purs`.
contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceScripts"
  validator <- alwaysSucceedsScriptV2
  let
    vhash :: ValidatorHash
    vhash = validatorHash validator

    scriptRef :: ScriptRef
    scriptRef = PlutusScriptRef (unwrap validator)

  logInfo' "Attempt to lock value"
  txId <- payWithScriptRefToAlwaysSucceeds vhash scriptRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId

payWithScriptRefToAlwaysSucceeds
  :: ValidatorHash -> ScriptRef -> Contract TransactionHash
payWithScriptRefToAlwaysSucceeds vhash scriptRef = do
  -- Send to own stake credential. This is used to test
  -- `mustPayToScriptAddressWithScriptRef`
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  let
    constraints :: TxConstraints
    constraints =
      case mbStakeKeyHash of
        Nothing ->
          Constraints.mustPayToScriptWithScriptRef vhash unitDatum DatumWitness
            scriptRef
            (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)
        Just stakeKeyHash ->
          Constraints.mustPayToScriptAddressWithScriptRef
            vhash
            (PubKeyCredential $ unwrap stakeKeyHash)
            unitDatum
            DatumWitness
            scriptRef
            (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromAlwaysSucceeds :: ValidatorHash -> TransactionHash -> Contract Unit
spendFromAlwaysSucceeds vhash txId = do
  -- Send to own stake credential. This is used to test
  -- `mustPayToScriptAddressWithScriptRef`
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  let
    scriptAddress =
      scriptHashAddress vhash (PubKeyCredential <<< unwrap <$> mbStakeKeyHash)
  utxos <- utxosAt scriptAddress

  txInput /\ txOutput <-
    liftContractM "Could not find unspent output locked at script address"
      $ find hasTransactionId (Map.toUnfoldable utxos :: Array _)

  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef txInput unitRedeemer
        (SpendInput $ mkTxUnspentOut txInput txOutput)

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId
