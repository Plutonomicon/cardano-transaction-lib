-- | A wrapper over `ByteArray` to indicate a byte array with no further specified meaning

module Types.RawBytes
  ( RawBytes(..)
  , rawBytesToByteArray
  , rawBytesFromByteArray
  , rawBytesToIntArray
  , rawBytesFromIntArray
  , rawBytesFromIntArrayUnsafe
  , rawBytesToHex
  , byteLength
  , hexToRawBytes
  , hexToRawBytesUnsafe
  ) where

import Data.Newtype (class Newtype, wrap, unwrap)

import Types.ByteArray (ByteArray)
import Types.ByteArray as BA
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
rawBytesToIntArray = BA.byteArrayToIntArray <<< unwrap

rawBytesFromIntArray :: Array Int -> Maybe RawBytes
rawBytesFromIntArray = map wrap <<< BA.byteArrayFromIntArray

rawBytesFromIntArrayUnsafe :: Array Int -> RawBytes
rawBytesFromIntArrayUnsafe = wrap <<< BA.byteArrayFromIntArrayUnsafe

rawBytesToHex :: RawBytes -> String
rawBytesToHex = BA.byteArrayToHex <<< unwrap

byteLength :: RawBytes -> Int
byteLength = BA.byteLength <<< unwrap

hexToRawBytes :: String -> Maybe RawBytes
hexToRawBytes = map wrap <<< BA.hexToByteArray

hexToRawBytesUnsafe :: String -> RawBytes
hexToRawBytesUnsafe = wrap <<< BA.hexToByteArrayUnsafe

rawBytesToByteArray :: RawBytes -> ByteArray
rawBytesToByteArray = unwrap

rawBytesFromByteArray :: ByteArray -> RawBytes
rawBytesFromByteArray = wrap

