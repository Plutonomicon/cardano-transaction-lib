-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Types.Natural
  ( (^-)
  , Natural
  , binaryOnBigInt
  , fromBigInt
  , fromBigInt'
  , minus
  , toBigInt
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Function (on)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import FromData (class FromData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Integer))

newtype Natural = Natural BigInt

derive newtype instance Eq Natural
derive newtype instance Ord Natural
derive newtype instance Semiring Natural

instance Show Natural where
  show (Natural n) = "(fromBigInt' (BigInt." <> show n <> "))"

instance FromData Natural where
  fromData (Integer n) = fromBigInt n
  fromData _ = Nothing

instance ToData Natural where
  toData (Natural n) = toData n

-- | Fails with `Nothing` on negative input.
fromBigInt :: BigInt -> Maybe Natural
fromBigInt n =
  if n >= BigInt.fromInt 0 then Just $ Natural n
  else Nothing

-- | Negate `BigInt` if it's negative.
fromBigInt' :: BigInt -> Natural
fromBigInt' n =
  if n >= BigInt.fromInt 0 then Natural n
  else Natural (negate n)

toBigInt :: Natural -> BigInt
toBigInt (Natural n) = n

-- | Use an arbitrary binary operation on the underlying `BigInt` for two
-- | natural numbers to return a natural number with potential failure if the
-- | output is not natural.
binaryOnBigInt
  :: (BigInt -> BigInt -> BigInt) -> Natural -> Natural -> Maybe Natural
binaryOnBigInt bin n = fromBigInt <<< (bin `on` toBigInt) n

-- | Subtracts one natural number from another via `BigInt`. If the number
-- | becomes negative, we return zero.
minus :: Natural -> Natural -> Natural
minus n = fromMaybe zero <<< binaryOnBigInt (-) n

-- Part of an `AdditiveHemigroup` notation but we'll use this for now.
infixl 6 minus as ^-