-- | A wrapper over `ByteArray` to indicate a byte array with no further specified meaning

module Types.RawBytes
  ( RawBytes(..)
  , rawBytesToByteArray
  , rawBytesFromByteArray
  , rawBytesToIntArray
  , rawBytesFromIntArray
  , rawBytesFromIntArrayUnsafe
  , rawBytesFromAscii
  , rawBytesToHex
  , byteLength
  , hexToRawBytes
  , hexToRawBytesUnsafe
  ) where

import Data.Newtype (class Newtype, wrap, unwrap)

import Types.ByteArray (ByteArray)
import Types.ByteArray as BytesArray
import Data.Maybe (Maybe)
import Prelude
import Aeson (class DecodeAeson, decodeAesonViaJson)
import Data.Argonaut (class DecodeJson, decodeJson)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

-- | An array of Bytes with no information about the content format
newtype RawBytes = RawBytes ByteArray

instance Show RawBytes where
  show rb = "(hexToRawBytesUnsafe " <> show (rawBytesToHex rb) <> ")"

derive instance Newtype RawBytes _

derive newtype instance Eq RawBytes
derive newtype instance Ord RawBytes
derive newtype instance Semigroup RawBytes
derive newtype instance Monoid RawBytes
derive newtype instance DecodeJson RawBytes
derive newtype instance DecodeAeson RawBytes
derive newtype instance Arbitrary RawBytes

rawBytesToIntArray :: RawBytes -> Array Int
rawBytesToIntArray = BytesArray.byteArrayToIntArray <<< unwrap

rawBytesFromIntArray :: Array Int -> Maybe RawBytes
rawBytesFromIntArray = map wrap <<< BytesArray.byteArrayFromIntArray

rawBytesFromIntArrayUnsafe :: Array Int -> RawBytes
rawBytesFromIntArrayUnsafe = wrap <<< BytesArray.byteArrayFromIntArrayUnsafe

rawBytesToHex :: RawBytes -> String
rawBytesToHex = BytesArray.byteArrayToHex <<< unwrap

byteLength :: RawBytes -> Int
byteLength = BytesArray.byteLength <<< unwrap

hexToRawBytes :: String -> Maybe RawBytes
hexToRawBytes = map wrap <<< BytesArray.hexToByteArray

hexToRawBytesUnsafe :: String -> RawBytes
hexToRawBytesUnsafe = wrap <<< BytesArray.hexToByteArrayUnsafe

rawBytesToByteArray :: RawBytes -> ByteArray
rawBytesToByteArray = unwrap

rawBytesFromByteArray :: ByteArray -> RawBytes
rawBytesFromByteArray = wrap

rawBytesFromAscii :: String -> Maybe RawBytes
rawBytesFromAscii = map wrap <<< BytesArray.byteArrayFromAscii
