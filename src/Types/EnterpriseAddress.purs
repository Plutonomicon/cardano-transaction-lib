module Cardano.Types.EnterpriseAddress where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(..)
  , decodeAeson
  , encodeAeson
  )
import Cardano.Serialization.Lib
  ( address_fromBech32
  , address_networkId
  , address_toBech32
  , enterpriseAddress_fromAddress
  , enterpriseAddress_new
  , enterpriseAddress_paymentCred
  , enterpriseAddress_toAddress
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Credential as Credential
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
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

data EnterpriseAddress = EnterpriseAddress NetworkId PaymentCredential

derive instance Generic EnterpriseAddress _
derive instance Ord EnterpriseAddress
derive instance Eq EnterpriseAddress

instance Show EnterpriseAddress where
  show = genericShow

-- no AsCbor instance, because there is no to_bytes method in CSL

instance EncodeAeson EnterpriseAddress where
  encodeAeson = toCsl >>> enterpriseAddress_toAddress
    >>> flip address_toBech32 (unsafeCoerce undefined)
    >>> encodeAeson

instance DecodeAeson EnterpriseAddress where
  decodeAeson = note (TypeMismatch "EnterpriseAddress") <<< decodeBech32 <=<
    decodeAeson
    where
    decodeBech32 = map fromCsl <<< toMaybe <<< enterpriseAddress_fromAddress <=<
      fromBech32

    fromBech32 :: Bech32String -> Maybe Csl.Address
    fromBech32 = toMaybe <<< address_fromBech32

toCsl :: EnterpriseAddress -> Csl.EnterpriseAddress
toCsl = case _ of
  EnterpriseAddress nid sc ->
    enterpriseAddress_new (Int.toNumber $ NetworkId.toInt nid)
      (Credential.toCsl $ unwrap sc)

fromCsl :: Csl.EnterpriseAddress -> EnterpriseAddress
fromCsl addr =
  EnterpriseAddress networkId
    (wrap $ Credential.fromCsl $ enterpriseAddress_paymentCred addr)
  where
  networkId :: NetworkId
  networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust
    $ Int.fromNumber
    $ address_networkId
    $ enterpriseAddress_toAddress addr
