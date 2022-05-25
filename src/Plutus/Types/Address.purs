module Plutus.Types.Address
  ( Address(Address)
  , AddressExtended(AddressExtended)
  , pubKeyHashAddress
  , scriptHashAddress
  , toPubKeyHash
  , toValidatorHash
  , toStakingCredential
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, wrap, unwrap)
import FromData (class FromData, fromData)
import Serialization.Address (NetworkId)
import ToData (class ToData, toData)
import Types.Scripts (ValidatorHash)
import Types.PlutusData (PlutusData(Constr))
import Types.PubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  , PubKeyHash
  , StakePubKeyHash
  )
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash)
  )

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

newtype AddressExtended = AddressExtended
  { address :: Address
  , networkId :: NetworkId
  }

derive instance Eq AddressExtended
derive instance Newtype AddressExtended _
derive instance Generic AddressExtended _

instance Show AddressExtended where
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

instance ToData Address where
  toData (Address a) = Constr zero $
    [ toData a.addressCredential, toData a.addressStakingCredential ]

instance FromData Address where
  fromData (Constr n [ credD, stakingCredD ]) | n == zero =
    Address <$>
      ( { addressCredential: _, addressStakingCredential: _ }
          <$> fromData credD
          <*> fromData stakingCredD
      )
  fromData _ = Nothing

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
