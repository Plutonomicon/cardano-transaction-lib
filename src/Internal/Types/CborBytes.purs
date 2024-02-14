-- | CborBytes. A wrapper over `ByteArray` to indicate that the bytes are cbor.
module Ctl.Internal.Types.CborBytes
  ( CborBytes(CborBytes)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Data.ByteArray (ByteArray)
import Data.ByteArray as ByteArray
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | An array of bytes containing CBOR data
newtype CborBytes = CborBytes ByteArray

instance Show CborBytes where
  show = genericShow

derive instance Newtype CborBytes _
derive instance Generic CborBytes _

derive newtype instance Eq CborBytes
derive newtype instance Ord CborBytes
derive newtype instance Semigroup CborBytes
derive newtype instance Monoid CborBytes
derive newtype instance EncodeAeson CborBytes
derive newtype instance DecodeAeson CborBytes
derive newtype instance Arbitrary CborBytes
