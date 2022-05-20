module Types.PubKeyHash
  ( PaymentPubKeyHash(..)
  , PubKeyHash(..)
  , StakePubKeyHash(..)
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  , payPubKeyHashRewardAddress
  , pubKeyHashBaseAddress
  , pubKeyHashEnterpriseAddress
  , pubKeyHashRewardAddress
  , stakePubKeyHashRewardAddress
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , getField
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import Serialization.Address
  ( Address
  , EnterpriseAddress
  , RewardAddress
  , NetworkId
  , baseAddressToAddress
  , enterpriseAddress
  , enterpriseAddressToAddress
  , keyHashCredential
  , pubKeyAddress
  , rewardAddress
  , rewardAddressToAddress
  )
import Serialization.Hash (Ed25519KeyHash)
import ToData (class ToData)

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

-- This is needed for `ApplyArgs`. Plutus has an `getPubKeyHash` field so don't
-- newtype derive.
instance DecodeAeson PubKeyHash where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getPubKeyHash" >=> decodeAeson >>> map PubKeyHash)

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

pubKeyHashBaseAddress :: NetworkId -> PubKeyHash -> StakePubKeyHash -> Address
pubKeyHashBaseAddress networkId pkh skh =
  baseAddressToAddress $ pubKeyAddress networkId (unwrap pkh) (unwrap skh)

pubKeyHashRewardAddress :: NetworkId -> PubKeyHash -> Address
pubKeyHashRewardAddress networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId

pubKeyHashEnterpriseAddress :: NetworkId -> PubKeyHash -> Address
pubKeyHashEnterpriseAddress networkId =
  enterpriseAddressToAddress <<< ed25519EnterpriseAddress networkId

newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow

-- This is needed for `ApplyArgs`. Plutus has an `unPaymentPubKeyHash` field so
-- don't newtype derive.
instance DecodeAeson PaymentPubKeyHash where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    ( flip getField "unPaymentPubKeyHash" >=>
        decodeAeson >>> map PaymentPubKeyHash
    )

newtype StakePubKeyHash = StakePubKeyHash Ed25519KeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance FromData StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

payPubKeyHashRewardAddress :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashRewardAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashRewardAddress networkId pkh

payPubKeyHashBaseAddress
  :: NetworkId -> PaymentPubKeyHash -> StakePubKeyHash -> Address
payPubKeyHashBaseAddress networkId (PaymentPubKeyHash pkh) skh =
  pubKeyHashBaseAddress networkId pkh skh

payPubKeyHashEnterpriseAddress :: NetworkId -> PaymentPubKeyHash -> Address
payPubKeyHashEnterpriseAddress networkId (PaymentPubKeyHash pkh) =
  pubKeyHashEnterpriseAddress networkId pkh

stakePubKeyHashRewardAddress :: NetworkId -> StakePubKeyHash -> Address
stakePubKeyHashRewardAddress networkId =
  rewardAddressToAddress <<< ed25519RewardAddress networkId
