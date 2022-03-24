-- | Our domain type for byte arrays, a wrapper over `Uint8Array`.
module Contract.Prim.ByteArray (module ByteArray, blake2bHash) where

import Prelude

import Contract.Monad (Contract(Contract))
import Data.Either (hush)
import Data.Maybe (Maybe)
import QueryM as QueryM
import Types.ByteArray (ByteArray)
import Types.ByteArray
  ( ByteArray(ByteArray)
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayFromString
  , byteArrayToHex
  , byteArrayToIntArray
  , byteLength
  , hexToByteArray
  , hexToByteArrayUnsafe
  ) as ByteArray

blake2bHash :: ByteArray -> Contract (Maybe ByteArray)
blake2bHash = Contract <<< map hush <<< QueryM.blake2bHash
