-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray
  ( module ByteArray
  , module CborBytes
  , module RawBytes
  ) where

import Cardano.Types.CborBytes
  ( CborBytes(CborBytes)
  ) as CborBytes
import Cardano.Types.RawBytes
  ( RawBytes(RawBytes)
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
