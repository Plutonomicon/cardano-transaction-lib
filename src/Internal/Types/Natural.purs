-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Ctl.Internal.Types.Natural
  ( (^-)
  , Natural
  , binaryOnBigInt
  , fromBigInt
  , fromBigInt'
  , fromInt
  , fromInt'
  , fromString
  , minus
  , toBigInt
  ) where

import Prelude

import Aeson (class DecodeAeson, JsonDecodeError(TypeMismatch), caseAesonBigInt)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Ctl.Internal.ToData (class ToData, toData)
import Ctl.Internal.Types.PlutusData (PlutusData(Integer))
import Data.Either (Either(Left), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, fromString) as BigInt

newtype Natural = Natural BigInt

derive newtype instance Eq Natural
derive newtype instance Ord Natural
derive newtype instance Semiring Natural
derive newtype instance FromMetadata Natural
derive newtype instance ToMetadata Natural

instance Show Natural where
  show (Natural n) = "(fromBigInt' (BigInt." <> show n <> "))"

instance FromData Natural where
  fromData (Integer n) = fromBigInt n
  fromData _ = Nothing

instance ToData Natural where
  toData (Natural n) = toData n

instance DecodeAeson Natural where
  decodeAeson =
    caseAesonBigInt
      (Left $ TypeMismatch "Expected BigInt from Aeson decoding")
      ( \bi -> note (TypeMismatch $ "Invalid Natural number: " <> show bi) $
          fromBigInt bi
      )

type MNatural = Maybe Natural

-- | Attempts to convert an instance of `Semiring` to `Natural`.
-- | Fails with `Nothing` on negative input.
fromSemiringType
  :: forall (a :: Type). Semiring a => Ord a => (a -> BigInt) -> a -> MNatural
fromSemiringType f n
  | n >= zero = Just (Natural $ f n)
  | otherwise = Nothing

-- | Converts an instance of `Semiring` to `Natural`.
-- | If the input is negative, negates it.
fromSemiringType'
  :: forall (a :: Type). Semiring a => Ord a => (a -> BigInt) -> a -> Natural
fromSemiringType' f n
  | n >= zero = Natural (f n)
  | otherwise = Natural (negate $ f n)

-- | Attempts to convert `BigInt` to `Natural`.
-- | Fails with `Nothing` on negative input.
fromBigInt :: BigInt -> Maybe Natural
fromBigInt = fromSemiringType identity

-- | Converts `BigInt` to `Natural`. Negates `BigInt` if it's negative.
fromBigInt' :: BigInt -> Natural
fromBigInt' = fromSemiringType' identity

-- | Unwraps `Natural` and returns the underlying `BigInt`.
toBigInt :: Natural -> BigInt
toBigInt (Natural n) = n

-- | Attempts to convert `Int` to `Natural`.
-- | Fails with `Nothing` on negative input.
fromInt :: Int -> Maybe Natural
fromInt = fromSemiringType BigInt.fromInt

-- | Converts `Int` to `Natural`. Negates `Int` if it's negative.
fromInt' :: Int -> Natural
fromInt' = fromSemiringType' BigInt.fromInt

-- | Attempts to build `Natural` from `String`.
fromString :: String -> Maybe Natural
fromString = fromBigInt <=< BigInt.fromString

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
