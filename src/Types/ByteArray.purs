-- | Our domain type for byte arrays, a wrapper over Uint8Array.
module Types.ByteArray
       ( ByteArray (..)
       , byteArrayFromIntArray
       , byteArrayFromIntArrayUnsafe
       , byteArrayToIntArray
       , byteArrayToHex
       , hexToByteArray
       , hexToByteArrayUnsafe
       )
where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ByteArray = ByteArray Uint8Array

instance showByteArray :: Show ByteArray where
  show arr = "(byteArrayFromIntArrayUnsafe " <> show (byteArrayToIntArray arr)  <> ")"

instance eqByteArray :: Eq ByteArray where
  eq a b = compare a b == EQ

instance ordByteArray :: Ord ByteArray where
  compare = \xs ys -> compare 0 (ord_ toDelta xs ys)
    where
    toDelta x y =
      case compare x y of
        EQ -> 0
        LT -> 1
        GT -> -1

instance semigroupByteArray :: Semigroup ByteArray where
  append = concat_

instance monoidByteArray :: Monoid ByteArray where
  mempty = byteArrayFromIntArrayUnsafe []

foreign import ord_ :: forall a. (Int -> Int -> Int) -> ByteArray -> ByteArray -> Int

foreign import concat_ :: ByteArray -> ByteArray -> ByteArray

foreign import byteArrayToHex :: ByteArray -> String

foreign import hexToByteArray_ :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> String -> Maybe ByteArray

-- | Input string must consist of hexademical numbers.
-- | Length of the input string must be even (2 characters per byte).
hexToByteArray :: String -> Maybe ByteArray
hexToByteArray = hexToByteArray_ Nothing Just

-- | Characters not in range will be converted to zero.
foreign import hexToByteArrayUnsafe :: String -> ByteArray

-- | Overflowing integers will be silently accepted modulo 256.
foreign import byteArrayFromIntArrayUnsafe :: Array Int -> ByteArray

foreign import byteArrayFromIntArray_ :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Array Int -> Maybe ByteArray

-- | A safer version of `byteArrayFromIntArrayUnsafe` that checks that elements are in range 0-255.
byteArrayFromIntArray :: Array Int -> Maybe ByteArray
byteArrayFromIntArray = byteArrayFromIntArray_ Nothing Just

foreign import byteArrayToIntArray :: ByteArray -> Array Int

instance arbitraryByteArray :: Arbitrary ByteArray where
  arbitrary = byteArrayFromIntArrayUnsafe <$> arbitrary
