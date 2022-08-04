module Plutip.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , keyWallets
  , encodeDistribution
  , transferFundsFromEnterpriseToBase
  , withStakeKey
  ) where

import Prelude

import Plutip.Types
  ( InitialUTxO
  , InitialUTxOWithStakeKey(InitialUTxOWithStakeKey)
  , PrivateKeyResponse(PrivateKeyResponse)
  , UtxoAmount
  )
import Contract.Address
  ( PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prelude (foldM, foldMap, null)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionOutput(TransactionOutput)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Wallet (withKeyWallet)
import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List, (:))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Plutus.Types.Transaction (Utxo)
import Type.Prelude (Proxy(Proxy))
import Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )

-- | A type class that implements a type-safe interface for specifying UTXO
-- | distribution for wallets.
-- | Number of wallets in distribution specification matches the number of
-- | wallets provided to the user.
class UtxoDistribution distr wallets | distr -> wallets where
  encodeDistribution :: distr -> Array (Array UtxoAmount)
  decodeWallets :: distr -> Array PrivateKeyResponse -> Maybe wallets
  keyWallets :: Proxy distr -> wallets -> Array KeyWallet

instance UtxoDistribution Unit Unit where
  encodeDistribution _ = []
  decodeWallets _ _ = pure unit
  keyWallets _ _ = []

instance UtxoDistribution InitialUTxO KeyWallet where
  encodeDistribution amounts = [ amounts ]
  decodeWallets _ [ PrivateKeyResponse key ] =
    Just $ privateKeysToKeyWallet (PrivatePaymentKey key) Nothing
  decodeWallets _ _ = Nothing
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution InitialUTxOWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOWithStakeKey _ amounts) = [ amounts ]
  decodeWallets (InitialUTxOWithStakeKey stake _) [ PrivateKeyResponse key ] =
    Just $ privateKeysToKeyWallet (PrivatePaymentKey key) (Just stake)
  decodeWallets _ _ = Nothing
  keyWallets _ wallet = [ wallet ]

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (headSpec /\ restSpec) (headWallets /\ restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets (distr /\ rest) =
    Array.uncons >=>
      \{ head, tail } -> do
        wallet <- decodeWallets distr [ head ]
        Tuple wallet <$> decodeWallets rest tail
  keyWallets _ (headWallets /\ restWallets) =
    (keyWallets (Proxy :: Proxy headSpec) headWallets)
      <> (keyWallets (Proxy :: Proxy restSpec) restWallets)

type WalletInfo =
  { utxos :: Utxo
  , payPkh :: PaymentPubKeyHash
  , stakePkh :: StakePubKeyHash
  , wallet :: KeyWallet
  }

-- | For each wallet which includes a stake key, transfer the value of
-- | the utxos at its enterprise address to its base address.
transferFundsFromEnterpriseToBase
  :: forall (r :: Row Type)
   . PrivatePaymentKey
  -> Array KeyWallet
  -> Contract r Unit
transferFundsFromEnterpriseToBase ourKey wallets = do
  -- Get all utxos and key hashes at all wallets containing a stake key
  walletsInfo <- foldM addStakeKeyWalletInfo mempty wallets
  unless (null walletsInfo) do
    let ourWallet = privateKeysToKeyWallet ourKey Nothing
    ourAddr <- liftedM "Could not get our address"
      $ withKeyWallet ourWallet getWalletAddress
    ourUtxos <- liftedM "Could not find our utxos"
      $ utxosAt ourAddr
    ourPkh <- liftedM "Could not get our payment pkh"
      $ withKeyWallet ourWallet ownPaymentPubKeyHash
    let
      lookups = Lookups.unspentOutputs (unwrap ourUtxos)
        <> foldMap (_.utxos >>> Lookups.unspentOutputs) walletsInfo

      constraints :: Constraints.TxConstraints Unit Unit
      constraints = Constraints.mustBeSignedBy ourPkh
        <> foldMap constraintsForWallet walletsInfo
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    signedTx <- liftedE $ withKeyWallet ourWallet $
      balanceAndSignTxE unbalancedTx
    signedTx' <- foldM
      ( \tx { wallet } -> liftedM "Could not sign" $ withKeyWallet wallet $
          signTransaction tx
      )
      (unwrap signedTx)
      walletsInfo
    txHash <- submit (wrap signedTx')
    awaitTxConfirmed txHash
  where
  constraintsForWallet :: WalletInfo -> Constraints.TxConstraints Unit Unit
  constraintsForWallet { utxos, payPkh, stakePkh } =
    -- TODO: It's necessary to include `mustBeSignedBy`, we get a
    -- `feeTooSmall` error otherwise. See
    -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/853
    Constraints.mustBeSignedBy payPkh <>
      foldMapWithIndex
        ( \input (TransactionOutput { amount }) ->
            Constraints.mustPayToPubKeyAddress payPkh stakePkh amount
              <> Constraints.mustSpendPubKeyOutput input
        )
        utxos

  addStakeKeyWalletInfo
    :: List WalletInfo
    -> KeyWallet
    -> Contract r (List WalletInfo)
  addStakeKeyWalletInfo walletsInfo wallet = withKeyWallet wallet $
    ownStakePubKeyHash >>= case _ of
      Nothing -> pure walletsInfo
      Just stakePkh -> do
        payPkh <- liftedM "Could not get payment pubkeyhash" $
          ownPaymentPubKeyHash
        networkId <- getNetworkId
        addr <- liftContractM "Could not get wallet address" $
          payPubKeyHashEnterpriseAddress networkId payPkh
        utxos' <- liftedM "Could not find utxos" $ utxosAt addr
        pure $ { utxos: unwrap utxos', payPkh, stakePkh, wallet } : walletsInfo

withStakeKey :: PrivateStakeKey -> InitialUTxO -> InitialUTxOWithStakeKey
withStakeKey = InitialUTxOWithStakeKey
