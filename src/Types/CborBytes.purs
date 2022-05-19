-- | CborBytes. A wrapper over `ByteArray` to indicate that the bytes are cbor.

module Types.CborBytes
  ( CborBytes(..)
  , cborBytesToByteArray
  , cborBytesFromByteArray
  , cborBytesFromAscii
  , cborBytesToIntArray
  , cborBytesFromIntArray
  , cborBytesFromIntArrayUnsafe
  , cborBytesToHex
  , byteLength
  , hexToCborBytes
  , hexToCborBytesUnsafe
  , rawBytesAsCborBytes
  ) where

import Data.Newtype (class Newtype, wrap, unwrap)

import Types.ByteArray (ByteArray)
import Types.ByteArray as BytesArray
import Types.RawBytes (RawBytes)
import Data.Maybe (Maybe)
import Prelude
import Aeson (class DecodeAeson)
import Data.Argonaut (class DecodeJson)
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | An array of Bytes containing CBOR data
newtype CborBytes = CborBytes ByteArray

instance Show CborBytes where
  show rb = "(hexToCborBytesUnsafe " <> show (cborBytesToHex rb) <>
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
cborBytesToIntArray = BytesArray.byteArrayToIntArray <<< unwrap

cborBytesFromIntArray :: Array Int -> Maybe CborBytes
cborBytesFromIntArray = map wrap <<< BytesArray.byteArrayFromIntArray

cborBytesFromIntArrayUnsafe :: Array Int -> CborBytes
cborBytesFromIntArrayUnsafe = wrap <<< BytesArray.byteArrayFromIntArrayUnsafe

cborBytesToHex :: CborBytes -> String
cborBytesToHex = BytesArray.byteArrayToHex <<< unwrap

byteLength :: CborBytes -> Int
byteLength = BytesArray.byteLength <<< unwrap

hexToCborBytes :: String -> Maybe CborBytes
hexToCborBytes = map wrap <<< BytesArray.hexToByteArray

hexToCborBytesUnsafe :: String -> CborBytes
hexToCborBytesUnsafe = wrap <<< BytesArray.hexToByteArrayUnsafe

cborBytesToByteArray :: CborBytes -> ByteArray
cborBytesToByteArray = unwrap

cborBytesFromByteArray :: ByteArray -> CborBytes
cborBytesFromByteArray = wrap

cborBytesFromAscii :: String -> Maybe CborBytes
cborBytesFromAscii = map wrap <<< BytesArray.byteArrayFromAscii

rawBytesAsCborBytes :: RawBytes -> CborBytes
rawBytesAsCborBytes = wrap <<< unwrap
