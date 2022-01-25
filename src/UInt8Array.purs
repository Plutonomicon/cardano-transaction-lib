module UInt8Array
  ( _byteLengthUint8Array
  , compareUint8Array
  , _emptyUint8Array
  , _eqUint8Array
  , _gtUint8Array
  , _ltUint8Array
  , _showUint8Array
  )
  where

import Prelude
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)

--------------------------------------------------------------------------------
-- Uint8Array helpers
--------------------------------------------------------------------------------
foreign import _byteLengthUint8Array :: Uint8Array -> BigInt

foreign import _emptyUint8Array :: Uint8Array

foreign import _eqUint8Array :: Uint8Array -> Uint8Array -> Boolean

foreign import _gtUint8Array :: Uint8Array -> Uint8Array -> Boolean

foreign import _ltUint8Array :: Uint8Array -> Uint8Array -> Boolean

foreign import _showUint8Array :: Uint8Array -> String

compareUint8Array :: Uint8Array -> Uint8Array -> Ordering
compareUint8Array b1 b2
  | _ltUint8Array b1 b2 = LT
  | _eqUint8Array b1 b2 = EQ
  | otherwise = GT
