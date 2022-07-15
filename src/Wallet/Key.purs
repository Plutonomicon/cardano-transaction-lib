module Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  ) where

import Prelude

import Cardano.Types.Transaction (Transaction(Transaction), _vkeys)
import Contract.Prelude (class Newtype)
import Data.Lens (set)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Deserialization.WitnessSet as Deserialization.WitnessSet
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Serialization (publicKeyFromPrivateKey, publicKeyHash)
import Serialization as Serialization
import Serialization.Address
  ( Address
  , NetworkId
  , baseAddress
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  )
import Serialization.Types (PrivateKey)

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------
type KeyWallet =
  { address :: NetworkId -> Aff Address
  , signTx :: Transaction -> Aff Transaction
  }

newtype PrivatePaymentKey = PrivatePaymentKey PrivateKey

derive instance Newtype PrivatePaymentKey _

newtype PrivateStakeKey = PrivateStakeKey PrivateKey

derive instance Newtype PrivateStakeKey _

privateKeysToKeyWallet
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey = { address, signTx }
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

  signTx :: Transaction -> Aff Transaction
  signTx (Transaction tx) = liftEffect do
    txBody <- Serialization.convertTxBody tx.body
    hash <- Serialization.hashTransaction txBody
    wit <- Deserialization.WitnessSet.convertVkeyWitness <$>
      Serialization.makeVkeywitness hash (unwrap payKey)
    let witnessSet' = set _vkeys (pure $ pure wit) mempty
    pure $ Transaction $ tx { witnessSet = witnessSet' <> tx.witnessSet }
