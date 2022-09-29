-- | A module with CBOR-related functionality.
module Contract.Cbor
  ( module Ctl.Internal.Types.Cbor
  , cborToHex
  , hexToCbor
  ) where

import Contract.Prelude

import Ctl.Internal.Types.Cbor (Cbor)
import Ctl.Internal.Types.CborBytes (cborBytesToHex, hexToCborBytes)
import Data.Newtype (unwrap, wrap)

-- | Decode a hexadecimal string into a `Cbor`.
-- | Results in `Nothing` if the string contains a non-hexadecimal number,
-- | or if the string is not even in length.
hexToCbor :: String -> Maybe Cbor
hexToCbor = map wrap <<< hexToCborBytes

-- | Encode a `Cbor` into a hexadecimal string.
cborToHex :: Cbor -> String
cborToHex = cborBytesToHex <<< unwrap