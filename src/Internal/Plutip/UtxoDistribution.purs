module Ctl.Internal.Plutip.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , decodeWallets'
  , keyWallets
  , encodeDistribution
  , transferFundsFromEnterpriseToBase
  , withStakeKey
  ) where

import Prelude

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
  ( TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmed
  , balanceAndSignTxE
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Wallet (withKeyWallet)
import Control.Alternative (guard)
import Control.Monad.Reader (asks)
import Ctl.Internal.Plutip.Types
  ( InitialUTxOs
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , PrivateKeyResponse(PrivateKeyResponse)
  , UtxoAmount
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List, (:))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type.Prelude (Proxy(Proxy))

-- | A type class that implements a type-safe interface for specifying UTXO
-- | distribution for wallets.
-- | Number of wallets in distribution specification matches the number of
-- | wallets provided to the user.
class UtxoDistribution distr wallets | distr -> wallets where
  encodeDistribution :: distr -> Array (Array UtxoAmount)
  decodeWallets :: distr -> Array PrivateKeyResponse -> Maybe wallets
  decodeWallets'
    :: distr
    -> Array PrivateKeyResponse
    -> Maybe (wallets /\ Array PrivateKeyResponse)
  keyWallets :: Proxy distr -> wallets -> Array KeyWallet

instance UtxoDistribution Unit Unit where
  encodeDistribution _ = []
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' _ pks = Just $ unit /\ pks
  keyWallets _ _ = []

instance UtxoDistribution InitialUTxOs KeyWallet where
  encodeDistribution amounts = [ amounts ]
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' _ pks = Array.uncons pks <#>
    \{ head: PrivateKeyResponse key, tail } ->
      (privateKeysToKeyWallet (PrivatePaymentKey key) Nothing) /\ tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution InitialUTxOsWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOsWithStakeKey _ amounts) = [ amounts ]
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (InitialUTxOsWithStakeKey stake _) pks = Array.uncons pks <#>
    \{ head: PrivateKeyResponse key, tail } ->
      privateKeysToKeyWallet (PrivatePaymentKey key) (Just stake) /\
        tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution (Array InitialUTxOs) (Array KeyWallet) where
  encodeDistribution amounts = amounts
  decodeWallets _ privateKeyResponses =
    Just $
      ( \(PrivateKeyResponse key) -> privateKeysToKeyWallet
          (PrivatePaymentKey key)
          Nothing
      ) <$> privateKeyResponses
  decodeWallets' listOfInitialUTxOs privateKeyResponses = do
    wallets <- traverse (\utxos -> decodeWallets utxos privateKeyResponses)
      listOfInitialUTxOs
    pure (wallets /\ privateKeyResponses)
  keyWallets _ wallets = wallets

instance UtxoDistribution (Array InitialUTxOsWithStakeKey) (Array KeyWallet) where
  encodeDistribution listOfInitialUTxOsWithStakeKey =
    (\(InitialUTxOsWithStakeKey _ amounts) -> amounts) <$>
      listOfInitialUTxOsWithStakeKey
  decodeWallets listOfInitialUTxOsWithStakeKey privateKeyResponses =
    Just $
      ( \(Tuple (PrivateKeyResponse key) (InitialUTxOsWithStakeKey stakeKey _)) ->
          privateKeysToKeyWallet
            (PrivatePaymentKey key)
            (Just stakeKey)
      ) <$> Array.zip privateKeyResponses listOfInitialUTxOsWithStakeKey
  decodeWallets' listOfInitialUTxOsWithStakeKey privateKeyResponses = do
    wallets <- traverse (\utxos -> decodeWallets utxos privateKeyResponses)
      listOfInitialUTxOsWithStakeKey
    pure (wallets /\ privateKeyResponses)
  keyWallets _ wallets = wallets

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (headSpec /\ restSpec) (headWallets /\ restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (distr /\ rest) pks = do
    (headWallets /\ pks') <- decodeWallets' distr pks
    (restWallets /\ pks'') <- decodeWallets' rest pks'
    pure $ (headWallets /\ restWallets) /\ pks''
  keyWallets _ (headWallets /\ restWallets) =
    keyWallets (Proxy :: Proxy headSpec) headWallets
      <> keyWallets (Proxy :: Proxy restSpec) restWallets

decodeWalletsDefault
  :: forall distr wallets
   . UtxoDistribution distr wallets
  => distr
  -> Array PrivateKeyResponse
  -> Maybe wallets
decodeWalletsDefault d p = do
  wallets /\ remainingPKeys <- decodeWallets' d p
  guard $ Array.null remainingPKeys
  pure wallets

type WalletInfo =
  { utxos :: UtxoMap
  , payPkh :: PaymentPubKeyHash
  , stakePkh :: StakePubKeyHash
  , wallet :: KeyWallet
  }

-- | For each wallet which includes a stake key, transfer the value of
-- | the utxos at its enterprise address to its base address. Note
-- | that this function clears the `usedTxOuts` cache, so it should
-- | not be used if there could be items in the cache that shouldn't
-- | be cleared (this function is intended to be used only on plutip
-- | startup).
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
      lookups :: Lookups.ScriptLookups Void
      lookups = Lookups.unspentOutputs ourUtxos
        <> foldMap (_.utxos >>> Lookups.unspentOutputs) walletsInfo

      constraints :: Constraints.TxConstraints Void Void
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
    -- Clear the used txouts cache because we know the state of these
    -- utxos is settled, see here:
    -- https://github.com/Plutonomicon/cardano-transaction-lib/pull/838#discussion_r941592493
    cache <- asks (unwrap <<< _.usedTxOuts <<< _.runtime <<< unwrap)
    liftEffect $ Ref.write Map.empty cache
  where
  constraintsForWallet :: WalletInfo -> Constraints.TxConstraints Void Void
  constraintsForWallet { utxos, payPkh, stakePkh } =
    -- It's necessary to include `mustBeSignedBy`, we get a
    -- `feeTooSmall` error otherwise. See
    -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/853
    Constraints.mustBeSignedBy payPkh <>
      foldMapWithIndex
        ( \input (TransactionOutputWithRefScript { output }) ->
            Constraints.mustPayToPubKeyAddress payPkh stakePkh
              (unwrap output).amount
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
        payPkh <- liftedM "Could not get payment pubkeyhash"
          ownPaymentPubKeyHash
        networkId <- getNetworkId
        addr <- liftContractM "Could not get wallet address" $
          payPubKeyHashEnterpriseAddress networkId payPkh
        utxos' <- liftedM "Could not find utxos" $ utxosAt addr
        pure $ { utxos: utxos', payPkh, stakePkh, wallet } : walletsInfo

withStakeKey :: PrivateStakeKey -> InitialUTxOs -> InitialUTxOsWithStakeKey
withStakeKey = InitialUTxOsWithStakeKey
