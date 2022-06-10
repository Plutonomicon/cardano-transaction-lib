module Plutus.Types.Address
  ( Address(Address)
  , AddressWithNetworkTag(AddressWithNetworkTag)
  , pubKeyHashAddress
  , scriptHashAddress
  , toPubKeyHash
  , toValidatorHash
  , toStakingCredential
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson'
  , (.:)
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Show.Generic (genericShow)
import FromData (class FromData, genericFromData)
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash)
  )
import Plutus.Types.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  )
import Serialization.Address (NetworkId)
import ToData (class ToData, genericToData)
import TypeLevel.Nat (Z)
import Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , StakePubKeyHash
  , PubKeyHash
  )
import Types.Scripts (ValidatorHash)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

newtype AddressWithNetworkTag = AddressWithNetworkTag
  { address :: Address
  , networkId :: NetworkId
  }

derive instance Eq AddressWithNetworkTag
derive instance Newtype AddressWithNetworkTag _
derive instance Generic AddressWithNetworkTag _

instance Show AddressWithNetworkTag where
  show = genericShow

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Tx.html#t:Address
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Address with two kinds of credentials, normal and staking.
newtype Address = Address
  { addressCredential :: Credential
  , addressStakingCredential :: Maybe StakingCredential
  }

derive instance Eq Address
derive instance Ord Address
derive instance Newtype Address _
derive instance Generic Address _

instance Show Address where
  show = genericShow

instance
  HasPlutusSchema
    Address
    ( "Address"
        :=
          ( "addressCredential" := I Credential :+ "addressStakingCredential"
              := I (Maybe StakingCredential)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData Address where
  toData = genericToData

instance FromData Address where
  fromData = genericFromData

instance DecodeAeson Address where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      addressCredential <- obj .: "addressCredential"
      addressStakingCredential <- obj .: "addressStakingCredential"
      pure $ Address { addressCredential, addressStakingCredential }

instance EncodeAeson Address where
  encodeAeson' (Address addr) = encodeAeson' addr

--------------------------------------------------------------------------------
-- Useful functions
--------------------------------------------------------------------------------

-- | The address that should be targeted by a transaction output locked
-- | by the public key with the given hash.
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Address
pubKeyHashAddress (PaymentPubKeyHash pkh) skh = wrap
  { addressCredential: PubKeyCredential pkh
  , addressStakingCredential:
      map (StakingHash <<< PubKeyCredential <<< unwrap) skh
  }

-- | The address that should be used by a transaction output locked
-- | by the given validator script hash.
scriptHashAddress :: ValidatorHash -> Address
scriptHashAddress vh = wrap
  { addressCredential: ScriptCredential vh
  , addressStakingCredential: Nothing
  }

-- | The PubKeyHash of the address (if any).
toPubKeyHash :: Address -> Maybe PubKeyHash
toPubKeyHash addr =
  case (unwrap addr).addressCredential of
    PubKeyCredential k -> Just k
    _ -> Nothing

-- | The validator hash of the address (if any).
toValidatorHash :: Address -> Maybe ValidatorHash
toValidatorHash addr =
  case (unwrap addr).addressCredential of
    ScriptCredential k -> Just k
    _ -> Nothing

-- | The staking credential of an address (if any).
toStakingCredential :: Address -> Maybe StakingCredential
toStakingCredential = _.addressStakingCredential <<< unwrap
