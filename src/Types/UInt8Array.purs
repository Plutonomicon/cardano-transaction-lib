module Types.UInt8Array
  ( _byteLengthUint8Array
  , _emptyUint8Array
  , _eqUint8Array
  , _showUint8Array
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)

--------------------------------------------------------------------------------
-- Uint8Array helpers
--------------------------------------------------------------------------------
foreign import _byteLengthUint8Array :: Uint8Array -> BigInt

foreign import _emptyUint8Array :: Uint8Array

foreign import _eqUint8Array :: Uint8Array -> Uint8Array -> Boolean

foreign import _showUint8Array :: Uint8Array -> String
