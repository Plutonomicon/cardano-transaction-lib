-- | A module with CBOR-related functionality.
module Contract.CborBytes
  ( module CborBytes
  , cborBytesToIntArray
  , cborBytesFromIntArray
  , cborBytesFromIntArrayUnsafe
  , cborBytesToHex
  , cborByteLength
  , hexToCborBytes
  , hexToCborBytesUnsafe
  , cborBytesToByteArray
  , cborBytesFromByteArray
  , cborBytesFromAscii
  , rawBytesAsCborBytes
  ) where

import Prelude

import Cardano.Types (CborBytes, RawBytes)
import Cardano.Types.CborBytes (CborBytes(CborBytes)) as CborBytes
import Data.ByteArray (ByteArray)
import Data.ByteArray as ByteArray
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)

cborBytesToIntArray :: CborBytes -> Array Int
cborBytesToIntArray = ByteArray.byteArrayToIntArray <<< unwrap

cborBytesFromIntArray :: Array Int -> Maybe CborBytes
cborBytesFromIntArray = map wrap <<< ByteArray.byteArrayFromIntArray

cborBytesFromIntArrayUnsafe :: Array Int -> CborBytes
cborBytesFromIntArrayUnsafe = wrap <<< ByteArray.byteArrayFromIntArrayUnsafe

cborBytesToHex :: CborBytes -> String
cborBytesToHex = ByteArray.byteArrayToHex <<< unwrap

cborByteLength :: CborBytes -> Int
cborByteLength = ByteArray.byteLength <<< unwrap

hexToCborBytes :: String -> Maybe CborBytes
hexToCborBytes = map wrap <<< ByteArray.hexToByteArray

hexToCborBytesUnsafe :: String -> CborBytes
hexToCborBytesUnsafe = wrap <<< ByteArray.hexToByteArrayUnsafe

cborBytesToByteArray :: CborBytes -> ByteArray
cborBytesToByteArray = unwrap

cborBytesFromByteArray :: ByteArray -> CborBytes
cborBytesFromByteArray = wrap

cborBytesFromAscii :: String -> Maybe CborBytes
cborBytesFromAscii = map wrap <<< ByteArray.byteArrayFromAscii

rawBytesAsCborBytes :: RawBytes -> CborBytes
rawBytesAsCborBytes = wrap <<< unwrap
