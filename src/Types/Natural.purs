-- | Arbitrary precision natural numbers (backed by `BigInt`).
module Types.Natural
  ( (^-)
  , Natural
  , binaryOnBigInt
  , fromBigInt
  , fromBigInt'
  , fromString
  , minus
  , toBigInt
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , caseAesonBigInt
  , decodeAeson
  , jsonToAeson
  )
import Data.Argonaut (class DecodeJson, JsonDecodeError(TypeMismatch))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(Integer))

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

-- This is needed for `ApplyArgs`.
instance DecodeJson Natural where
  decodeJson = decodeAeson <<< jsonToAeson

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
