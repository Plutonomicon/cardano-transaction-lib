-- | CborBytes. A wrapper over `ByteArray` to indicate that the bytes are cbor.

module Types.CborBytes
  ( CborBytes(..)
  , cborBytesToIntArray
  , cborBytesFromIntArray
  , cborBytesFromIntArrayUnsafe
  , cborBytesToHex
  , byteLength
  , hexToCborBytes
  , hexToCborBytesUnsafe
  ) where

import Data.Newtype (class Newtype, wrap, unwrap)

import Types.ByteArray (ByteArray)
import Types.ByteArray as BA
import Data.Maybe (Maybe)
import Prelude
import Aeson (class DecodeAeson, decodeAesonViaJson)
import Data.Argonaut (class DecodeJson, decodeJson)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype CborBytes = CborBytes ByteArray

instance Show CborBytes where
  show rb = "(cborBytesFromIntArrayUnsafe" <> show (cborBytesToIntArray rb) <> ")"

derive instance Newtype CborBytes _

derive instance Eq CborBytes

derive instance Ord CborBytes

instance Semigroup CborBytes where
  append r1 r2 = wrap $ unwrap r1 `append` unwrap r2

instance Monoid CborBytes where
  mempty = cborBytesFromIntArrayUnsafe []

instance DecodeJson CborBytes where
  decodeJson = map wrap <<< decodeJson

instance DecodeAeson CborBytes where
  decodeAeson = decodeAesonViaJson

instance Arbitrary CborBytes where
  arbitrary = wrap <$> arbitrary

cborBytesToIntArray :: CborBytes -> Array Int
cborBytesToIntArray = BA.byteArrayToIntArray <<< unwrap

cborBytesFromIntArray :: Array Int -> Maybe CborBytes
cborBytesFromIntArray = map wrap <<< BA.byteArrayFromIntArray

cborBytesFromIntArrayUnsafe :: Array Int -> CborBytes
cborBytesFromIntArrayUnsafe = wrap <<< BA.byteArrayFromIntArrayUnsafe

cborBytesToHex :: CborBytes -> String
cborBytesToHex = BA.byteArrayToHex <<< unwrap

byteLength :: CborBytes -> Int
byteLength = BA.byteLength <<< unwrap

hexToCborBytes :: String -> Maybe CborBytes
hexToCborBytes = map wrap <<< BA.hexToByteArray

hexToCborBytesUnsafe :: String -> CborBytes
hexToCborBytesUnsafe = wrap <<< BA.hexToByteArrayUnsafe


