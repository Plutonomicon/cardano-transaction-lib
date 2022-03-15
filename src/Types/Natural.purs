-- | Arbitrary precision natural number (backed by BigInt).
module Types.Natural
  ( Natural
  , fromBigInt
  , fromBigInt'
  , toBigInt
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))

newtype Natural = Natural BigInt

derive newtype instance Eq Natural
derive newtype instance Ord Natural

instance Show Natural where
  show (Natural n) = "(fromBigInt' (BigInt." <> show n <> "))"

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
