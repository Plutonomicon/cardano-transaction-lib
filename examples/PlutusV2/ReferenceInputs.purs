module Ctl.Examples.PlutusV2.ReferenceInputs (contract, example, main) where

import Contract.Prelude

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
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet
  ( getWalletUtxos
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
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
    (Array.head <$> ownPaymentPubKeyHashes)
  skh <- join <<< Array.head <$> ownStakePubKeyHashes

  utxos <- liftedM "Failed to get UTxOs from wallet" getWalletUtxos
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

  void $ runChecks checks $ lift do
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    balancedSignedTx <- signTransaction =<< liftedE (balanceTx unbalancedTx)
    txHash <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txHash
    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"

    pure { referenceInput: oref, balancedSignedTx }

type ContractResult =
  { referenceInput :: TransactionInput
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
  \{ referenceInput } -> do
    let
      assertionFailure :: ContractAssertionFailure
      assertionFailure = CustomFailure "Reference input has been spent"
    utxos <- lift $ liftedM "Failed to get UTxOs from wallet" getWalletUtxos
    assertContract assertionFailure do
      Map.member referenceInput utxos

checks :: Array (ContractCheck ContractResult)
checks =
  [ assertTxContainsReferenceInput
  , assertReferenceInputNotSpent
  ]
