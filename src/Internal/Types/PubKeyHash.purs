module Ctl.Internal.Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash(PubKeyHash)
  , StakePubKeyHash(StakePubKeyHash)
  , payPubKeyHashBaseAddressImpl
  , payPubKeyHashEnterpriseAddressImpl
  , payPubKeyHashRewardAddressImpl
  , pubKeyHashBaseAddressImpl
  , pubKeyHashEnterpriseAddressImpl
  , pubKeyHashRewardAddressImpl
  , stakePubKeyHashRewardAddressImpl
  , ed25519RewardAddress
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , (.:)
  )
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.Serialization.Address
  ( Address
  , EnterpriseAddress
  , NetworkId
  , RewardAddress
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  , paymentKeyHashStakeKeyHashAddress
  , rewardAddress
  , rewardAddressToAddress
  )
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Ctl.Internal.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance FromData PubKeyHash
derive newtype instance FromMetadata PubKeyHash
derive newtype instance Ord PubKeyHash
derive newtype instance ToData PubKeyHash
derive newtype instance ToMetadata PubKeyHash

instance Show PubKeyHash where
  show = genericShow

instance EncodeAeson PubKeyHash where
  encodeAeson x = encodeAeson { getPubKeyHash: unwrap x }

instance DecodeAeson PubKeyHash where
  decodeAeson a = do
    obj <- decodeAeson a
    wrap <$> obj .: "getPubKeyHash"

ed25519EnterpriseAddress
  :: forall (n :: Type)
   . Newtype n Ed25519KeyHash
  => NetworkId
  -> n
  -> EnterpriseAddress
ed25519EnterpriseAddress network pkh =
  enterpriseAddress
    { network
    , paymentCred: keyHashCredential (unwrap pkh)
    }

ed25519RewardAddress
  :: forall (n :: Type)
   . Newtype n Ed25519KeyHash
  => NetworkId
  -> n
  -> RewardAddress
ed25519RewardAddress network skh =
  rewardAddress
    { network
    , paymentCred: keyHashCredential (unwrap skh)
    }

pubKeyHashBaseAddressImpl
  :: NetworkId -> PubKeyHash -> StakePubKeyHash -> Address
pubKeyHashBaseAddressImpl networkId pkh skh =
  baseAddressToAddress $ paymentKeyHashStakeKeyHashAddress networkId
    (unwrap pkh)
    (unwrap $ unwrap skh)

pubKeyHashRewardAddressImpl :: NetworkId -> PubKeyHash -> Address
pubKeyHashRewardAddressImpl networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId

pubKeyHashEnterpriseAddressImpl :: NetworkId -> PubKeyHash -> Address
pubKeyHashEnterpriseAddressImpl networkId =
  enterpriseAddressToAddress <<< ed25519EnterpriseAddress networkId

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash

instance EncodeAeson PaymentPubKeyHash where
  encodeAeson (PaymentPubKeyHash pkh) = encodeAeson
    { "unPaymentPubKeyHash": pkh }

instance DecodeAeson PaymentPubKeyHash where
  decodeAeson json = do
    obj <- decodeAeson json
    PaymentPubKeyHash <<< PubKeyHash <$> obj .: "unPaymentPubKeyHash"

instance Show PaymentPubKeyHash where
  show = genericShow

newtype StakePubKeyHash = StakePubKeyHash PubKeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance FromData StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

payPubKeyHashRewardAddressImpl :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashRewardAddressImpl networkId (PaymentPubKeyHash pkh) =
  pubKeyHashRewardAddressImpl networkId pkh

payPubKeyHashBaseAddressImpl
  :: NetworkId -> PaymentPubKeyHash -> StakePubKeyHash -> Address
payPubKeyHashBaseAddressImpl networkId (PaymentPubKeyHash pkh) skh =
  pubKeyHashBaseAddressImpl networkId pkh skh

payPubKeyHashEnterpriseAddressImpl :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashEnterpriseAddressImpl networkId (PaymentPubKeyHash pkh) =
  pubKeyHashEnterpriseAddressImpl networkId pkh

stakePubKeyHashRewardAddressImpl :: NetworkId -> StakePubKeyHash -> Address
stakePubKeyHashRewardAddressImpl networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId <<< unwrap
