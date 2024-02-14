-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray
  ( module ByteArray
  , module CborBytes
  , module RawBytes
  ) where

import Ctl.Internal.Types.CborBytes
  ( CborBytes(CborBytes)
  ) as CborBytes
import Ctl.Internal.Types.RawBytes
  ( RawBytes(RawBytes)
  , hexToRawBytes
  , hexToRawBytesUnsafe
  , rawBytesFromAscii
  , rawBytesFromByteArray
  , rawBytesFromIntArray
  , rawBytesFromIntArrayUnsafe
  , rawBytesToByteArray
  , rawBytesToHex
  , rawBytesToIntArray
  ) as RawBytes
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
