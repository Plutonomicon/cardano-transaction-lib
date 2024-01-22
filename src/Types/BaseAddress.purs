module Cardano.Types.BaseAddress where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(..), decodeAeson, encodeAeson)
import Cardano.Serialization.Lib (address_fromBech32, address_networkId, address_toBech32, baseAddress_fromAddress, baseAddress_new, baseAddress_paymentCred, baseAddress_stakeCred, baseAddress_toAddress)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
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

data BaseAddress = BaseAddress NetworkId PaymentCredential StakeCredential

derive instance Generic BaseAddress _
derive instance Eq BaseAddress
derive instance Ord BaseAddress

instance Show BaseAddress where
  show = genericShow

-- no AsCbor instance, because there is no to_bytes method in CSL

instance EncodeAeson BaseAddress where
  encodeAeson = toCsl >>> baseAddress_toAddress >>> flip address_toBech32 (unsafeCoerce undefined) >>> encodeAeson

instance DecodeAeson BaseAddress where
  decodeAeson = note (TypeMismatch "BaseAddress") <<< decodeBech32 <=< decodeAeson
    where
      decodeBech32 = map fromCsl <<< toMaybe <<< baseAddress_fromAddress <=< fromBech32
      fromBech32 :: Bech32String -> Maybe Csl.Address
      fromBech32 = toMaybe <<< address_fromBech32

toCsl :: BaseAddress -> Csl.BaseAddress
toCsl (BaseAddress nid pc sc) =
    baseAddress_new
      (Int.toNumber $ NetworkId.toInt nid)
      (unwrap pc)
      (unwrap sc)

fromCsl :: Csl.BaseAddress -> BaseAddress
fromCsl addr =
  BaseAddress networkId  (wrap $ baseAddress_paymentCred addr)
  (wrap $ baseAddress_stakeCred addr)
  where
    networkId :: NetworkId
    networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust $ Int.fromNumber $ address_networkId $ baseAddress_toAddress addr
