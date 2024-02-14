module Cardano.Types.PointerAddress where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.Serialization.Lib
  ( fromBytes
  , pointerAddress_fromAddress
  , pointerAddress_toAddress
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Ctl.Internal.Helpers (compareViaCslBytes, eqOrd)
import Data.ByteArray (byteArrayToHex)
import Data.Either (note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (toMaybe)

newtype PointerAddress = PointerAddress Csl.PointerAddress

derive instance Generic PointerAddress _
derive instance Newtype PointerAddress _

instance Eq PointerAddress where
  eq = eqOrd

instance Ord PointerAddress where
  compare = compareViaCslBytes `on` (unwrap >>> pointerAddress_toAddress)

instance Show PointerAddress where
  -- TODO: make this instance lawful
  show addr = show $ byteArrayToHex $ toBytes $ pointerAddress_toAddress
    (unwrap addr)

instance EncodeAeson PointerAddress where
  encodeAeson = encodeAeson <<< toBytes <<< pointerAddress_toAddress <<< unwrap

instance DecodeAeson PointerAddress where
  decodeAeson addr = decodeAeson addr >>= \bytes ->
    note (TypeMismatch "PointerAddress") $ PointerAddress <$> do
      address <- fromBytes bytes
      toMaybe $ pointerAddress_fromAddress address
