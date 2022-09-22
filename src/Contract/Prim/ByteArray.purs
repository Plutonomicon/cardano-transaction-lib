-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray (module ByteArray) where

import Ctl.Internal.Types.ByteArray
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
