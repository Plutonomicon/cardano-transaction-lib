module Examples.BalanceTxConstraints
  ( ContractParams(ContractParams)
  , contract
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getWalletAddressWithNetworkTag
  , ownPaymentPubKeyHash
  )
import Contract.BalanceTxConstraints
  ( BalanceTxConstraintsBuilder
  , mustBalanceTxWithAddress'
  , mustGenChangeOutsWithMaxTokenQuantity
  ) as BalanceTxConstraints
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.Test.Utils
  ( ContractAssertionFailure(CustomFailure)
  , ContractBasicAssertion
  , label
  )
import Contract.Test.Utils as TestUtils
import Contract.Transaction
  ( Transaction
  , TransactionHash
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (valueOf, singleton) as Value
import Contract.Wallet (KeyWallet, withKeyWallet)
import Data.Array (sort) as Array
import Data.BigInt (BigInt, fromInt)
import Examples.AlwaysMints (alwaysMintsPolicy)
import Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers

newtype ContractParams = ContractParams
  { aliceKeyWallet :: KeyWallet
  , bobKeyWallet :: KeyWallet
  }

type ContractResult =
  { txHash :: TransactionHash
  , changeAddress :: Address
  , mintedToken :: CurrencySymbol /\ TokenName
  }

assertChangeOutputsPartitionedCorrectly
  :: ContractBasicAssertion () ContractResult Unit
assertChangeOutputsPartitionedCorrectly
  { txHash, changeAddress: addr, mintedToken: cs /\ tn } = do
  let labeledAddr = label addr "changeAddress"
  TestUtils.runContractAssertionM' $
    TestUtils.checkNewUtxosAtAddress labeledAddr txHash \changeOutputs -> do
      let
        assertionFailure :: ContractAssertionFailure
        assertionFailure =
          CustomFailure "Change outputs were not partitioned correctly"

      TestUtils.assertContract assertionFailure do
        let
          values :: Array Value
          values =
            changeOutputs <#> _.amount <<< unwrap <<< _.output <<< unwrap

          tokenQuantities :: Array BigInt
          tokenQuantities =
            Array.sort $ values <#> \v -> Value.valueOf v cs tn

        tokenQuantities == map fromInt [ 3, 4, 4 ]

contract :: ContractParams -> Contract () Unit
contract (ContractParams p) = do
  logInfo' "Examples.BalanceTxConstraints"

  alicePubKeyHash <-
    liftedM "Failed to get own PKH" ownPaymentPubKeyHash

  bobPubKeyHash <-
    liftedM "Failed to get Bob's PKH"
      (withKeyWallet p.bobKeyWallet ownPaymentPubKeyHash)

  bobAddress <-
    liftedM "Failed to get Bob's address"
      (withKeyWallet p.bobKeyWallet getWalletAddressWithNetworkTag)

  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "The Token"
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValue (Value.singleton cs tn $ fromInt 11)
        <> foldMap Constraints.mustBeSignedBy [ alicePubKeyHash, bobPubKeyHash ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

    balanceTxConstraints :: BalanceTxConstraints.BalanceTxConstraintsBuilder
    balanceTxConstraints =
      BalanceTxConstraints.mustGenChangeOutsWithMaxTokenQuantity (fromInt 4)
        <> BalanceTxConstraints.mustBalanceTxWithAddress' bobAddress

  void $ TestUtils.withAssertions assertChangeOutputsPartitionedCorrectly do
    unbalancedTx <-
      liftedE $ Lookups.mkUnbalancedTx lookups constraints

    balancedTx <-
      liftedE $ map unwrap <$>
        balanceTxWithConstraints balanceTxConstraints unbalancedTx

    balancedSignedTx <-
      foldM signWithWallet balancedTx [ p.aliceKeyWallet, p.bobKeyWallet ]

    txHash <- submit (wrap balancedSignedTx)
    logInfo' $ "Tx ID: " <> show txHash

    awaitTxConfirmed txHash
    logInfo' "Tx submitted successfully!"

    let changeAddress = (unwrap bobAddress).address
    pure { txHash, changeAddress, mintedToken: cs /\ tn }
  where
  signWithWallet :: Transaction -> KeyWallet -> Contract () Transaction
  signWithWallet txToSign wallet =
    liftedM "Failed to sign transaction"
      (withKeyWallet wallet $ signTransaction txToSign)

