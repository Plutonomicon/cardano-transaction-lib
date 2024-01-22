module Cardano.Types.PointerAddress where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(..), decodeAeson, encodeAeson)
import Cardano.Serialization.Lib (byronAddress_fromBase58, byronAddress_toBase58, fromBytes, pointerAddress_fromAddress, pointerAddress_toAddress, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Ctl.Internal.Helpers (compareViaCslBytes, eqOrd, showFromBytes)
import Ctl.Internal.Types.Aliases (Base58String)
import Data.ByteArray (byteArrayFromIntArrayUnsafe, byteArrayToHex)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Coarbitrary, coarbitrary)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype PointerAddress = PointerAddress Csl.PointerAddress

derive instance Generic PointerAddress _
derive instance Newtype PointerAddress _

instance Eq PointerAddress where
  eq = eqOrd

instance Ord PointerAddress where
  compare = compareViaCslBytes `on` (unwrap >>> pointerAddress_toAddress)

instance Show PointerAddress where
  -- TODO: make this instance lawful
  show addr = show $ byteArrayToHex $ toBytes $ pointerAddress_toAddress (unwrap addr)

instance EncodeAeson PointerAddress where
  encodeAeson = encodeAeson <<< toBytes <<< pointerAddress_toAddress <<< unwrap

instance DecodeAeson PointerAddress where
  decodeAeson addr = decodeAeson addr >>= \bytes ->
    note (TypeMismatch "PointerAddress") $ PointerAddress <$> do
      address <- fromBytes bytes
      toMaybe $ pointerAddress_fromAddress address
