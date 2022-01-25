-- | Our domain type for byte arrays, a wrapper over Uint8Array.
module Types.ByteArray
       ( ByteArray (..)
       , hexToByteArray
       , byteArrayToHex
       , hexToByteArrayUnsafe
       , byteArrayFromIntArray
       , byteArrayToIntArray
       )
where

import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering(..))
import Prelude
import Data.ArrayBuffer.Types (Uint8Array)

newtype ByteArray = ByteArray Uint8Array

instance showByteArray :: Show ByteArray where
  show arr = "(byteArrayFromIntArray " <> show (byteArrayToIntArray arr)  <> ")"

instance eqByteArray :: Eq ByteArray where
  eq a b = compare a b == EQ

instance ordByteArray :: Ord ByteArray where
  compare = \xs ys -> compare 0 (ordImpl toDelta xs ys)
    where
    toDelta x y =
      case compare x y of
        EQ -> 0
        LT -> 1
        GT -> -1

instance semigroupByteArray :: Semigroup ByteArray where
  append = concatImpl

instance monoidByteArray :: Monoid ByteArray where
  mempty = byteArrayFromIntArray []

foreign import ordImpl :: forall a. (Int -> Int -> Int) -> ByteArray -> ByteArray -> Int

foreign import concatImpl :: ByteArray -> ByteArray -> ByteArray

foreign import byteArrayToHex :: ByteArray -> String

foreign import hexToByteArrayImpl :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> String -> Maybe ByteArray

-- | Input string must consist of hexademical numbers.
-- | Length of the input string must be even (2 characters per byte).
hexToByteArray :: String -> Maybe ByteArray
hexToByteArray = hexToByteArrayImpl Nothing Just

-- | Characters not in range will be converted to zero.
foreign import hexToByteArrayUnsafe :: String -> ByteArray

-- | Overflowing integers will be silently accepted modulo 256.
foreign import byteArrayFromIntArray :: Array Int -> ByteArray

foreign import byteArrayToIntArray :: ByteArray -> Array Int
