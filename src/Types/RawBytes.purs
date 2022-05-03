-- | A wrapper over `ByteArray` to indicate a byte array with no further specified meaning

module Types.RawBytes
  ( RawBytes(..)
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

newtype RawBytes = RawBytes ByteArray

instance Show RawBytes where
  show rb = "(rawBytesFromIntArrayUnsafe" <> show (rawBytesToIntArray rb) <> ")"

derive instance Newtype RawBytes _

derive instance Eq RawBytes

derive instance Ord RawBytes

instance Semigroup RawBytes where
  append r1 r2 = wrap $ unwrap r1 `append` unwrap r2

instance Monoid RawBytes where
  mempty = rawBytesFromIntArrayUnsafe []

instance DecodeJson RawBytes where
  decodeJson = map wrap <<< decodeJson

instance DecodeAeson RawBytes where
  decodeAeson = decodeAesonViaJson

instance Arbitrary RawBytes where
  arbitrary = wrap <$> arbitrary

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


