-- | CborBytes. A wrapper over `ByteArray` to indicate that the bytes are cbor.

module Types.CborBytes
  ( CborBytes(..)
  , cborBytesToByteArray
  , cborBytesFromByteArray
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

-- | An array of Bytes containing CBOR data
newtype CborBytes = CborBytes ByteArray

instance Show CborBytes where
  show rb = "(cborBytesFromIntArrayUnsafe" <> show (cborBytesToIntArray rb) <>
    ")"

derive instance Newtype CborBytes _

derive newtype instance Eq CborBytes
derive newtype instance Ord CborBytes
derive newtype instance Semigroup CborBytes
derive newtype instance Monoid CborBytes
derive newtype instance DecodeJson CborBytes
derive newtype instance DecodeAeson CborBytes
derive newtype instance Arbitrary CborBytes

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

cborBytesToByteArray :: CborBytes -> ByteArray
cborBytesToByteArray = unwrap

cborBytesFromByteArray :: ByteArray -> CborBytes
cborBytesFromByteArray = wrap
