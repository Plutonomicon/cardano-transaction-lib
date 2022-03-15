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
import FromData (class FromData)
import ToData (class ToData, toData)
import Types.PlutusData (PlutusData(..))

newtype Natural = Natural BigInt

derive newtype instance Eq Natural
derive newtype instance Ord Natural

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
