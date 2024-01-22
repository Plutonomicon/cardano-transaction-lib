module Cardano.Types.RewardAddress where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(..), decodeAeson, encodeAeson)
import Cardano.Serialization.Lib (address_fromBech32, address_networkId, address_toBech32, rewardAddress_fromAddress, rewardAddress_new, rewardAddress_paymentCred, rewardAddress_toAddress)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.StakeCredential (StakeCredential)
import Ctl.Internal.Types.Aliases (Bech32String)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Literals.Undefined (undefined)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data RewardAddress = RewardAddress NetworkId StakeCredential

derive instance Generic RewardAddress _
derive instance Ord RewardAddress
derive instance Eq RewardAddress

instance Show RewardAddress where
  show = genericShow

-- no AsCbor instance, because there is no to_bytes method in CSL

instance EncodeAeson RewardAddress where
  encodeAeson = toCsl >>> rewardAddress_toAddress >>> flip address_toBech32 (unsafeCoerce undefined) >>> encodeAeson

instance DecodeAeson RewardAddress where
  decodeAeson = note (TypeMismatch "RewardAddress") <<< decodeBech32 <=< decodeAeson
    where
      decodeBech32 = map fromCsl <<< toMaybe <<< rewardAddress_fromAddress <=< fromBech32
      fromBech32 :: Bech32String -> Maybe Csl.Address
      fromBech32 = toMaybe <<< address_fromBech32

toCsl :: RewardAddress -> Csl.RewardAddress
toCsl = case _ of
  RewardAddress nid sc ->
    rewardAddress_new (Int.toNumber $ NetworkId.toInt nid) (unwrap sc)

fromCsl :: Csl.RewardAddress -> RewardAddress
fromCsl addr =
  RewardAddress networkId (wrap $ rewardAddress_paymentCred addr)
  where
    networkId :: NetworkId
    networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust $ Int.fromNumber $ address_networkId $ rewardAddress_toAddress addr
