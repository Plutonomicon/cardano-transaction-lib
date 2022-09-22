module Examples.BalanceTxConstraints
  ( ContractParams(ContractParams)
  , contract
  ) where

import Contract.Prelude

import Contract.Address
  ( getWalletAddressWithNetworkTag
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
import Contract.Transaction
  ( Transaction
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (singleton) as Value
import Contract.Wallet (KeyWallet, withKeyWallet)
import Data.BigInt (fromInt)
import Examples.AlwaysMints (alwaysMintsPolicy)
import Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers

newtype ContractParams = ContractParams
  { aliceKeyWallet :: KeyWallet
  , bobKeyWallet :: KeyWallet
  }

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
  where
  signWithWallet :: Transaction -> KeyWallet -> Contract () Transaction
  signWithWallet txToSign wallet =
    liftedM "Failed to sign transaction"
      (withKeyWallet wallet $ signTransaction txToSign)

