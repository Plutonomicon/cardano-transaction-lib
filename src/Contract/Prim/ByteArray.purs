-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray
  ( module ByteArray
  , module CborBytes
  , module RawBytes
  , rawBytesToByteArray
  , rawBytesFromByteArray
  , rawBytesToIntArray
  , rawBytesFromIntArray
  , rawBytesFromIntArrayUnsafe
  , rawBytesFromAscii
  , rawBytesToHex
  , hexToRawBytes
  , hexToRawBytesUnsafe
  ) where

import Prelude

import Cardano.Types.CborBytes (CborBytes(CborBytes)) as CborBytes
import Cardano.Types.RawBytes (RawBytes)
import Cardano.Types.RawBytes (RawBytes(RawBytes)) as RawBytes
import Data.ByteArray (ByteArray)
import Data.ByteArray
  ( ByteArray(ByteArray)
  , byteArrayFromAscii
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , byteArrayToIntArray
  , byteLength
  , hexToByteArray
  , hexToByteArrayUnsafe
  ) as ByteArray
import Data.ByteArray as BytesArray
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)

rawBytesToIntArray :: RawBytes -> Array Int
rawBytesToIntArray = BytesArray.byteArrayToIntArray <<< unwrap

rawBytesFromIntArray :: Array Int -> Maybe RawBytes
rawBytesFromIntArray = map wrap <<< BytesArray.byteArrayFromIntArray

rawBytesFromIntArrayUnsafe :: Array Int -> RawBytes
rawBytesFromIntArrayUnsafe = wrap <<< BytesArray.byteArrayFromIntArrayUnsafe

rawBytesToHex :: RawBytes -> String
rawBytesToHex = BytesArray.byteArrayToHex <<< unwrap

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
