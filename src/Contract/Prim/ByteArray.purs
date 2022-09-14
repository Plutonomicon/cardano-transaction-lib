-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module CTL.Contract.Prim.ByteArray (module ByteArray) where

import CTL.Internal.Types.ByteArray
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
