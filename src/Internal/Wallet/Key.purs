module Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  ) where

import Prelude

import Contract.Prelude (class Newtype)
import Ctl.Internal.BalanceTx.Collateral.Select (selectCollateral) as Collateral
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet
  , UtxoMap
  , _vkeys
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  )
import Ctl.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Ctl.Internal.QueryM.Ogmios (CoinsPerUtxoUnit)
import Ctl.Internal.Serialization
  ( privateKeySign
  , publicKeyFromPrivateKey
  , publicKeyHash
  )
import Ctl.Internal.Serialization as Serialization
import Ctl.Internal.Serialization.Address
  ( Address
  , NetworkId
  , baseAddress
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Data.Array (fromFoldable)
import Data.Lens (set)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------
newtype KeyWallet = KeyWallet
  { address :: NetworkId -> Aff Address
  , selectCollateral ::
      CoinsPerUtxoUnit
      -> Int
      -> UtxoMap
      -> Effect (Maybe (Array TransactionUnspentOutput))
  , signTx :: Transaction -> Aff TransactionWitnessSet
  , signData :: RawBytes -> Aff DataSignature
  , paymentKey :: PrivatePaymentKey
  , stakeKey :: Maybe PrivateStakeKey
  }

derive instance Newtype KeyWallet _

newtype PrivatePaymentKey = PrivatePaymentKey PrivateKey

derive instance Newtype PrivatePaymentKey _

newtype PrivateStakeKey = PrivateStakeKey PrivateKey

derive instance Newtype PrivateStakeKey _

keyWalletPrivatePaymentKey :: KeyWallet -> PrivatePaymentKey
keyWalletPrivatePaymentKey = unwrap >>> _.paymentKey

keyWalletPrivateStakeKey :: KeyWallet -> Maybe PrivateStakeKey
keyWalletPrivateStakeKey = unwrap >>> _.stakeKey

privateKeysToKeyWallet
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey = KeyWallet
  { address
  , selectCollateral
  , signTx
  , signData
  , paymentKey: payKey
  , stakeKey: mbStakeKey
  }
  where
  address :: NetworkId -> Aff Address
  address network = do
    pubPayKey <- liftEffect $ publicKeyFromPrivateKey (unwrap payKey)
    case mbStakeKey of
      Just stakeKey -> do
        pubStakeKey <- liftEffect $ publicKeyFromPrivateKey (unwrap stakeKey)
        pure $ baseAddressToAddress $
          baseAddress
            { network
            , paymentCred: keyHashCredential $ publicKeyHash $ pubPayKey
            , delegationCred: keyHashCredential $ publicKeyHash $ pubStakeKey
            }

      Nothing -> pure $ pubPayKey # publicKeyHash
        >>> keyHashCredential
        >>> { network, paymentCred: _ }
        >>> enterpriseAddress
        >>> enterpriseAddressToAddress

  selectCollateral
    :: CoinsPerUtxoUnit
    -> Int
    -> UtxoMap
    -> Effect (Maybe (Array TransactionUnspentOutput))
  selectCollateral coinsPerUtxoByte maxCollateralInputs utxos = map fromFoldable
    <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs utxos

  signTx :: Transaction -> Aff TransactionWitnessSet
  signTx (Transaction tx) = liftEffect do
    txBody <- Serialization.convertTxBody tx.body
    hash <- Serialization.hashTransaction txBody
    wit <- Deserialization.WitnessSet.convertVkeyWitness <$>
      Serialization.makeVkeywitness hash (unwrap payKey)
    let witnessSet' = set _vkeys (pure $ pure wit) mempty
    pure witnessSet'

  signData :: RawBytes -> Aff DataSignature
  signData dat = do
    encoded <- liftEffect $ (privateKeySign <<< unwrap) payKey $
        (unwrap <<< unwrap) dat
    pure $ { key: encoded, signature: encoded }

