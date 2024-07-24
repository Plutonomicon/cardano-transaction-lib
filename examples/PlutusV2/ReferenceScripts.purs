module Ctl.Examples.PlutusV2.ReferenceScripts
  ( main
  , example
  , contract
  ) where

import Contract.Prelude

import Cardano.Types
  ( Credential(ScriptHashCredential)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.PlutusData (unit) as PlutusData
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  )
import Contract.Credential (Credential(PubKeyHashCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.PlutusData (unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput(TransactionInput)
  , awaitTxConfirmed
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

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec = Just $ ConnectToGenericCip30 "nami" { cip95: false }
  }

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
    scriptRef = PlutusScriptRef validator

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
          Constraints.mustPayToScriptWithScriptRef vhash PlutusData.unit
            DatumWitness
            scriptRef
            (Value.lovelaceValueOf $ BigNum.fromInt 2_000_000)
        Just stakeKeyHash ->
          Constraints.mustPayToScriptAddressWithScriptRef
            vhash
            (PubKeyHashCredential $ unwrap stakeKeyHash)
            PlutusData.unit
            DatumWitness
            scriptRef
            (Value.lovelaceValueOf $ BigNum.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  submitTxFromConstraints lookups constraints

spendFromAlwaysSucceeds :: ValidatorHash -> TransactionHash -> Contract Unit
spendFromAlwaysSucceeds vhash txId = do
  -- Send to own stake credential. This is used to test
  -- `mustPayToScriptAddressWithScriptRef`
  mbStakeKeyHash <- join <<< head <$> ownStakePubKeyHashes
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential $ vhash)
    (wrap <<< PubKeyHashCredential <<< unwrap <$> mbStakeKeyHash)
  utxos <- utxosAt scriptAddress

  input /\ output <-
    liftContractM "Could not find unspent output locked at script address"
      $ find hasTransactionId (Map.toUnfoldable utxos :: Array _)

  let
    constraints :: TxConstraints
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef input unitRedeemer
        (SpendInput $ TransactionUnspentOutput { input, output })

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.datum PlutusData.unit

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId
