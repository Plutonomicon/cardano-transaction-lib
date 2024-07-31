module Ctl.Internal.Test.UtxoDistribution
  ( class UtxoDistribution
  , decodeWallets
  , decodeWallets'
  , keyWallets
  , encodeDistribution
  , transferFundsFromEnterpriseToBase
  , withStakeKey
  , InitialUTxOs
  , InitialUTxODistribution
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , TestWalletSpec(TestWalletSpec)
  , UtxoAmount
  ) where

import Prelude

import Cardano.Types
  ( BigNum
  , Credential(PubKeyHashCredential)
  , PaymentCredential(PaymentCredential)
  , PaymentPubKeyHash
  , StakePubKeyHash
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.Address (Address(EnterpriseAddress))
import Cardano.Types.PrivateKey (PrivateKey)
import Cardano.Types.UtxoMap (UtxoMap)
import Cardano.Wallet.Key
  ( KeyWallet
  , PrivateDrepKey
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Contract.Address (getNetworkId)
import Contract.Monad (Contract, liftedM)
import Contract.Prelude (foldM, foldMap, null)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (utxosAt)
import Contract.Wallet
  ( getWalletAddresses
  , mkKeyWalletFromPrivateKeys
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Control.Alternative (guard)
import Control.Monad.Reader (asks)
import Control.Monad.State.Trans (StateT(StateT), runStateT)
import Data.Array (head)
import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type.Prelude (Proxy(Proxy))

-- | UTxO amount in Lovelaces
type UtxoAmount = BigNum

-- | A list of UTxOs for a single wallet
type InitialUTxOs = Array UtxoAmount

-- | A wrapper that allows to specify a stake key to attach to a
-- | generated pre-funded Address.
data InitialUTxOsWithStakeKey =
  InitialUTxOsWithStakeKey PrivateStakeKey InitialUTxOs

newtype TestWalletSpec = TestWalletSpec
  { utxos :: Array UtxoAmount
  , stakeKey :: Maybe PrivateStakeKey
  , drepKey :: Maybe PrivateDrepKey
  }

derive instance Generic TestWalletSpec _
derive instance Newtype TestWalletSpec _

instance Show TestWalletSpec where
  show = genericShow

-- | A spec for distribution of UTxOs between wallets.
type InitialUTxODistribution = Array InitialUTxOs

-- | A type class that implements a type-safe interface for specifying UTXO
-- | distribution for wallets.
-- | Number of wallets in distribution specification matches the number of
-- | wallets provided to the user.
class UtxoDistribution distr wallets | distr -> wallets where
  encodeDistribution :: distr -> Array (Array UtxoAmount)
  decodeWallets :: distr -> Array PrivateKey -> Maybe wallets
  decodeWallets'
    :: distr
    -> Array PrivateKey
    -> Maybe (wallets /\ Array PrivateKey)
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
    \{ head: key, tail } ->
      (privateKeysToKeyWallet (PrivatePaymentKey key) Nothing Nothing) /\ tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution InitialUTxOsWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOsWithStakeKey _ amounts) = [ amounts ]
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (InitialUTxOsWithStakeKey stake _) pks = Array.uncons pks <#>
    \{ head: key, tail } ->
      privateKeysToKeyWallet (PrivatePaymentKey key) (Just stake) Nothing /\
        tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution TestWalletSpec KeyWallet where
  encodeDistribution (TestWalletSpec { utxos }) = [ utxos ]
  decodeWallets distr privateKeys = decodeWalletsDefault distr privateKeys
  decodeWallets' (TestWalletSpec { stakeKey, drepKey }) privateKeys =
    Array.uncons privateKeys <#> \{ head: key, tail } ->
      privateKeysToKeyWallet (PrivatePaymentKey key) stakeKey drepKey /\
        tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution (Array InitialUTxOs) (Array KeyWallet) where
  encodeDistribution = encodeDistributionArray
  decodeWallets d = decodeWalletsDefault d
  decodeWallets' = decodeWallets'Array
  keyWallets = keyWalletsArray

instance UtxoDistribution (Array InitialUTxOsWithStakeKey) (Array KeyWallet) where
  encodeDistribution = encodeDistributionArray
  decodeWallets d = decodeWalletsDefault d
  decodeWallets' = decodeWallets'Array
  keyWallets = keyWalletsArray

instance UtxoDistribution (Array TestWalletSpec) (Array KeyWallet) where
  encodeDistribution = encodeDistributionArray
  decodeWallets d = decodeWalletsDefault d
  decodeWallets' = decodeWallets'Array
  keyWallets = keyWalletsArray

encodeDistributionArray
  :: forall (distr :: Type)
   . UtxoDistribution distr KeyWallet
  => Array distr
  -> Array (Array UtxoAmount)
encodeDistributionArray = (_ >>= encodeDistribution)

decodeWallets'Array
  :: forall (distr :: Type)
   . UtxoDistribution distr KeyWallet
  => Array distr
  -> Array PrivateKey
  -> Maybe (Array KeyWallet /\ Array PrivateKey)
decodeWallets'Array = runStateT <<< traverse (StateT <<< decodeWallets')

keyWalletsArray
  :: forall (distr :: Type)
   . Proxy distr
  -> Array KeyWallet
  -> Array KeyWallet
keyWalletsArray _ wallets = wallets

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (Tuple headSpec restSpec)
    (Tuple headWallets restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (distr /\ rest) = runStateT do
    headWallets <- StateT $ decodeWallets' distr
    restWallets <- StateT $ decodeWallets' rest
    pure (headWallets /\ restWallets)
  keyWallets _ (headWallets /\ restWallets) =
    keyWallets (Proxy :: Proxy headSpec) headWallets
      <> keyWallets (Proxy :: Proxy restSpec) restWallets

decodeWalletsDefault
  :: forall distr wallets
   . UtxoDistribution distr wallets
  => distr
  -> Array PrivateKey
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
-- | be cleared (this function is intended to be used only on `ContractTest`
-- | startup).
transferFundsFromEnterpriseToBase
  :: PrivatePaymentKey
  -> Array KeyWallet
  -> Contract Unit
transferFundsFromEnterpriseToBase ourKey wallets = do
  -- Get all utxos and key hashes at all wallets containing a stake key
  walletsInfo <- foldM addStakeKeyWalletInfo mempty wallets
  unless (null walletsInfo) do
    let ourWallet = mkKeyWalletFromPrivateKeys ourKey Nothing Nothing
    ourAddr <- liftedM "Could not get our address" $
      head <$> withKeyWallet ourWallet getWalletAddresses
    ourUtxos <- utxosAt ourAddr
    ourPkh <- liftedM "Could not get our payment pkh" $
      head <$> withKeyWallet ourWallet ownPaymentPubKeyHashes
    let
      lookups :: Lookups.ScriptLookups
      lookups = Lookups.unspentOutputs ourUtxos
        <> foldMap (_.utxos >>> Lookups.unspentOutputs) walletsInfo
        <> Lookups.ownPaymentPubKeyHash ourPkh

      constraints :: Constraints.TxConstraints
      constraints = Constraints.mustBeSignedBy ourPkh
        <> foldMap constraintsForWallet walletsInfo
    unbalancedTx /\ usedUtxos <- mkUnbalancedTx lookups constraints
    signedTx <-
      withKeyWallet ourWallet $
        signTransaction =<< balanceTx unbalancedTx usedUtxos mempty
    signedTx' <- foldM
      (\tx { wallet } -> withKeyWallet wallet $ signTransaction tx)
      signedTx
      walletsInfo
    txHash <- submit signedTx'
    awaitTxConfirmed txHash
    -- Clear the used txouts cache because we know the state of these
    -- utxos is settled, see here:
    -- https://github.com/Plutonomicon/cardano-transaction-lib/pull/838#discussion_r941592493
    cache <- asks (unwrap <<< _.usedTxOuts)
    liftEffect $ Ref.write Map.empty cache
  where
  constraintsForWallet :: WalletInfo -> Constraints.TxConstraints
  constraintsForWallet { utxos, payPkh, stakePkh } =
    -- It's necessary to include `mustBeSignedBy`, we get a
    -- `feeTooSmall` error otherwise. See
    -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/853
    Constraints.mustBeSignedBy payPkh
      <> Constraints.mustBeSignedBy (wrap $ unwrap stakePkh)
      <> foldMapWithIndex
        ( \input (TransactionOutput { amount }) ->
            Constraints.mustPayToPubKeyAddress payPkh stakePkh amount
              <> Constraints.mustSpendPubKeyOutput input
        )
        utxos

  addStakeKeyWalletInfo
    :: List WalletInfo
    -> KeyWallet
    -> Contract (List WalletInfo)
  addStakeKeyWalletInfo walletsInfo wallet = withKeyWallet wallet $
    join <<< head <$> ownStakePubKeyHashes >>= case _ of
      Nothing -> pure walletsInfo
      Just stakePkh -> do
        payPkh <- liftedM "Could not get payment pubkeyhash" $
          head <$> ownPaymentPubKeyHashes
        networkId <- getNetworkId
        utxos' <- utxosAt $ EnterpriseAddress
          { networkId
          , paymentCredential: PaymentCredential $ PubKeyHashCredential $ unwrap
              payPkh
          }
        pure $ { utxos: utxos', payPkh, stakePkh, wallet } : walletsInfo

withStakeKey :: PrivateStakeKey -> InitialUTxOs -> InitialUTxOsWithStakeKey
withStakeKey = InitialUTxOsWithStakeKey
