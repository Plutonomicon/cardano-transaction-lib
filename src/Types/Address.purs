module Cardano.Types.Address where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(..), decodeAeson, encodeAeson)
import Cardano.Serialization.Lib (address_fromBech32, address_networkId, address_toBech32, baseAddress_fromAddress, baseAddress_toAddress, byronAddress_fromAddress, byronAddress_toAddress, enterpriseAddress_fromAddress, enterpriseAddress_new, enterpriseAddress_paymentCred, enterpriseAddress_toAddress, fromBytes, pointerAddress_fromAddress, pointerAddress_toAddress, rewardAddress_fromAddress, rewardAddress_toAddress, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.BaseAddress as BA
import Cardano.Types.ByronAddress (ByronAddress)
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
import Cardano.Types.PointerAddress (PointerAddress) as PA
import Cardano.Types.RewardAddress as RA
import Control.Alt ((<|>))
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

data Address
  = BaseAddress BA.BaseAddress
  | ByronAddress ByronAddress
  | EnterpriseAddress NetworkId PaymentCredential
  | RewardAddress RA.RewardAddress
  | PointerAddress PA.PointerAddress

derive instance Generic Address _
derive instance Eq Address
derive instance Ord Address

instance EncodeAeson Address where
  encodeAeson = encodeAeson <<< toBech32

instance DecodeAeson Address where
  decodeAeson = decodeAeson >=> fromBech32 >>> note (TypeMismatch "Address")

instance Show Address where
  show = genericShow

instance AsCbor Address where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

toBech32 :: Address -> Bech32String
toBech32 = toCsl >>> flip address_toBech32 (unsafeCoerce undefined)

fromBech32 :: Bech32String -> Maybe Address
fromBech32 = map fromCsl <<< toMaybe <<< address_fromBech32

toCsl :: Address -> Csl.Address
toCsl = case _ of
  BaseAddress ba ->
    baseAddress_toAddress $ BA.toCsl ba
  ByronAddress ba -> byronAddress_toAddress $ unwrap ba
  EnterpriseAddress nid pc ->
    enterpriseAddress_toAddress $ enterpriseAddress_new (Int.toNumber $ NetworkId.toInt nid) (unwrap pc)
  RewardAddress ra ->
    rewardAddress_toAddress $ RA.toCsl ra
  PointerAddress pc -> pointerAddress_toAddress $ unwrap pc

fromCsl :: Csl.Address -> Address
fromCsl addr =
  unsafePartial $ fromJust $
  asBaseAddress <|> asByronAddress <|> asEnterpriseAddress <|> asRewardAddress <|> asPointerAddress
  where
    networkId :: NetworkId
    networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust $ Int.fromNumber $ address_networkId addr
    asBaseAddress =
      toMaybe (baseAddress_fromAddress addr) <#>
        BaseAddress <<< BA.fromCsl
    asByronAddress = ByronAddress <<< wrap <$> toMaybe (byronAddress_fromAddress addr)
    asEnterpriseAddress = toMaybe (enterpriseAddress_fromAddress addr) <#> \cslEnterpriseAddress ->
      EnterpriseAddress networkId (wrap $ enterpriseAddress_paymentCred cslEnterpriseAddress)
    asRewardAddress = toMaybe (rewardAddress_fromAddress addr) <#>
      RewardAddress <<< RA.fromCsl
    asPointerAddress = toMaybe (pointerAddress_fromAddress addr) <#> PointerAddress <<< wrap
