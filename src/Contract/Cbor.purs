module Contract.Cbor
  ( hexToByteArray
  , hexToCbor
  , hexToCborBytes
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Data.Newtype (wrap)
import Effect.Exception (error)
import Types.ByteArray (ByteArray)
import Types.ByteArray (hexToByteArray) as BA
import Types.Cbor (Cbor)
import Types.CborBytes (CborBytes)

hexToByteArray :: String -> Contract () ByteArray
hexToByteArray hex =
  liftM (error $ "Failed conversion of CBOR hex to ByteArray") $
    BA.hexToByteArray hex

hexToCborBytes :: String -> Contract () CborBytes
hexToCborBytes hex = do
  byteArray <- hexToByteArray hex
  pure $ wrap byteArray

hexToCbor :: String -> Contract () Cbor
hexToCbor hex = do
  cborBytes <- hexToCborBytes hex
  pure $ wrap cborBytes