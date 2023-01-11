module Ctl.Examples.PlutusV2.ReferenceInputs (contract, example, main) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getWalletAddresses
  , ownPaymentPubKeysHashes
  , ownStakePubKeysHashes
  )
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedE
  , liftedM
  , runContract
  )
import Contract.ScriptLookups as Lookups
import Contract.Test.Assert
  ( ContractAssertionFailure(CustomFailure)
  , ContractCheck
  , assertContract
  , assertionToCheck
  , runChecks
  )
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionInput
  , _body
  , _referenceInputs
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf) as Value
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.Helpers (mustPayToPubKeyStakeAddress) as Helpers
import Data.Array (head) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Lens.Getter ((^.))
import Data.Map (member, toUnfoldable) as Map
import Data.Set (member) as Set

main :: Effect Unit
main = example testnetNamiConfig

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceInputs"

  pkh <- liftedM "Failed to get own PKH"
    (Array.head <$> ownPaymentPubKeysHashes)
  skh <- join <<< Array.head <$> ownStakePubKeysHashes

  ownAddress <- liftedM "Failed to get own address"
    (Array.head <$> getWalletAddresses)
  utxos <- utxosAt ownAddress
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Map.toUnfoldable utxos :: Array _))

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Constraints.mustReferenceOutput oref
      , Helpers.mustPayToPubKeyStakeAddress pkh skh
          (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  void $ runChecks checks do
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    balancedSignedTx <- signTransaction =<< liftedE (balanceTx unbalancedTx)
    txHash <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txHash
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"

    pure { ownAddress, referenceInput: oref, balancedSignedTx }

type ContractResult =
  { ownAddress :: Address
  , referenceInput :: TransactionInput
  , balancedSignedTx :: BalancedSignedTransaction
  }

assertTxContainsReferenceInput :: ContractCheck ContractResult
assertTxContainsReferenceInput =
  assertionToCheck "Tx contains a reference input"
    \{ balancedSignedTx, referenceInput } -> do
      let
        assertionFailure :: ContractAssertionFailure
        assertionFailure = CustomFailure
          "Could not find given input in `referenceInputs`"
      assertContract assertionFailure do
        Set.member referenceInput
          (unwrap balancedSignedTx ^. _body <<< _referenceInputs)

assertReferenceInputNotSpent :: ContractCheck ContractResult
assertReferenceInputNotSpent = assertionToCheck "A reference input UTxO"
  \{ ownAddress, referenceInput } -> do
    let
      assertionFailure :: ContractAssertionFailure
      assertionFailure = CustomFailure "Reference input has been spent"
    utxos <- lift $ utxosAt ownAddress
    assertContract assertionFailure do
      Map.member referenceInput utxos

checks :: Array (ContractCheck ContractResult)
checks =
  [ assertTxContainsReferenceInput
  , assertReferenceInputNotSpent
  ]
