-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray (module ByteArray) where

import Types.ByteArray
  ( ByteArray(ByteArray)
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayFromAscii
  , byteArrayToHex
  , byteArrayToIntArray
  , byteLength
  , hexToByteArray
  , hexToByteArrayUnsafe
  ) as ByteArray
