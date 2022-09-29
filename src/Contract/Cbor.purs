module Contract.Cbor
  ( module CborBytes
  , hexToCbor
  ) where

import Contract.Prelude

import Ctl.Internal.Types.Cbor (Cbor)
import Ctl.Internal.Types.CborBytes (hexToCborBytes) as CborBytes
import Data.Newtype (wrap)

-- | Decode a hexadecimal string into a `Cbor`.
-- | Results in `Nothing` if the string contains a non-hexadecimal number,
-- | or if the string is not even in length.
hexToCbor :: String -> Maybe Cbor
hexToCbor = map wrap <<< CborBytes.hexToCborBytes