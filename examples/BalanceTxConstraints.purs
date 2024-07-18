module Ctl.Examples.BalanceTxConstraints
  ( ContractParams(ContractParams)
  , contract
  ) where

import Contract.Prelude

import Cardano.Types (Asset(Asset), BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Address (Address)
import Contract.BalanceTxConstraints
  ( BalancerConstraints
  , mustGenChangeOutsWithMaxTokenQuantity
  , mustNotSpendUtxoWithOutRef
  , mustSendChangeToAddress
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Assert
  ( ContractAssertionFailure(CustomFailure)
  , ContractCheck
  , assertContract
  , assertNewUtxosAtAddress
  , assertionToCheck
  , label
  , runChecks
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (valueOf) as Value
import Contract.Wallet
  ( KeyWallet
  , getWalletAddresses
  , getWalletCollateral
  , ownPaymentPubKeyHashes
  , withKeyWallet
  )
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Array (head)
import Data.Array (sort) as Array
import Data.Map (fromFoldable, keys, member) as Map
import Data.Set (findMin) as Set
import JS.BigInt as BigInt

newtype ContractParams = ContractParams
  { aliceKeyWallet :: KeyWallet
  , bobKeyWallet :: KeyWallet
  }

type ContractResult =
  { txHash :: TransactionHash
  , changeAddress :: Address
  , nonSpendableAddress :: Address
  , mintedToken :: CurrencySymbol /\ TokenName
  , nonSpendableOref :: TransactionInput
  }

-- | Checks that the resultant change outputs of the transaction are partitioned
-- | correctly, i.e. their token quantities do not exceed the specified upper
-- | limit of 4 tokens per change output.
assertChangeOutputsPartitionedCorrectly
  :: ContractCheck ContractResult
assertChangeOutputsPartitionedCorrectly = assertionToCheck
  "Change is correctly partitioned"
  \{ txHash, changeAddress: addr, mintedToken: cs /\ tn } -> do
    let labeledAddr = label addr "changeAddress"
    assertNewUtxosAtAddress labeledAddr txHash \changeOutputs -> do
      let
        assertionFailure :: ContractAssertionFailure
        assertionFailure =
          CustomFailure "Change outputs were not partitioned correctly"

      assertContract assertionFailure do
        let
          values :: Array Value
          values =
            changeOutputs <#> _.amount <<< unwrap

          tokenQuantities :: Array BigNum
          tokenQuantities =
            Array.sort $ values <#> \v -> Value.valueOf (Asset cs tn) v

        tokenQuantities == map BigNum.fromInt [ 3, 4, 4 ]

-- | Checks that the utxo with the specified output reference
-- | (`nonSpendableOref`) is not consumed during transaction balancing.
assertSelectedUtxoIsNotSpent
  :: ContractCheck ContractResult
assertSelectedUtxoIsNotSpent =
  assertionToCheck "Non-spendable UTxO hasn't been spent"
    \{ nonSpendableAddress, nonSpendableOref } -> do
      utxos <- lift $ utxosAt nonSpendableAddress
      let
        assertionFailure :: ContractAssertionFailure
        assertionFailure =
          CustomFailure "The utxo marked as non-spendable has been spent"

      assertContract assertionFailure $
        Map.member nonSpendableOref utxos

checks :: Array (ContractCheck ContractResult)
checks =
  [ assertChangeOutputsPartitionedCorrectly
  , assertSelectedUtxoIsNotSpent
  ]

contract :: ContractParams -> Contract Unit
contract (ContractParams p) = do
  logInfo' "Examples.BalanceTxConstraints"

  aliceAddress <-
    liftedM "Failed to get Alice's address"
      $ head
      <$> (withKeyWallet p.aliceKeyWallet getWalletAddresses)

  alicePubKeyHash <-
    liftedM "Failed to get own PKH" $ head <$> ownPaymentPubKeyHashes

  bobPubKeyHash <-
    liftedM "Failed to get Bob's PKH"
      $ head
      <$> (withKeyWallet p.bobKeyWallet ownPaymentPubKeyHashes)

  bobAddress <-
    liftedM "Failed to get Bob's address"
      $ head
      <$> (withKeyWallet p.bobKeyWallet getWalletAddresses)

  bobsCollateralArray <- withKeyWallet p.bobKeyWallet do
    fold <$> getWalletCollateral
  let
    bobsCollateral =
      Map.fromFoldable $ bobsCollateralArray <#> unwrap >>>
        \{ input, output } -> Tuple input output

  nonSpendableOref <-
    liftedM "Failed to get utxos at Alice's address"
      (Set.findMin <<< Map.keys <$> utxosAt aliceAddress)
  mp <- alwaysMintsPolicy
  let cs = PlutusScript.hash mp
  tn <- Helpers.mkAssetName "The Token"
  let
    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue (Mint.singleton cs tn $ Int.fromInt 11)
        <> foldMap Constraints.mustBeSignedBy [ alicePubKeyHash, bobPubKeyHash ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.plutusMintingPolicy mp

    balanceTxConstraints :: BalancerConstraints
    balanceTxConstraints =
      mustGenChangeOutsWithMaxTokenQuantity
        (BigInt.fromInt 4)
        <> mustUseUtxosAtAddress bobAddress
        <> mustSendChangeToAddress bobAddress
        <> mustNotSpendUtxoWithOutRef nonSpendableOref
        <> mustUseCollateralUtxos bobsCollateral

  void $ runChecks checks $ lift do
    unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints

    balancedTx <- balanceTx unbalancedTx usedUtxos balanceTxConstraints

    balancedSignedTx <-
      (withKeyWallet p.bobKeyWallet <<< signTransaction)
        =<< withKeyWallet p.aliceKeyWallet (signTransaction balancedTx)

    txHash <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txHash

    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"

    let changeAddress = bobAddress
    pure
      { txHash
      , changeAddress
      , nonSpendableAddress: aliceAddress
      , mintedToken: cs /\ tn
      , nonSpendableOref
      }
