-- | `cardano-serialization-lib` Int type (can be positive or negative).
module Types.Int
  ( Int
  , newPositive
  , newNegative
  , fromBigInt
  ) where

import Prelude

import Prim as Prim
import Serialization.Types (BigNum)
import Data.Function (on)
import Data.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe, fromJust)
import Control.Alternative ((<|>))
import Serialization.BigNum (bigNumFromBigInt)

foreign import data Int :: Prim.Type

foreign import newPositive :: BigNum -> Int
foreign import newNegative :: BigNum -> Int
foreign import _intToStr :: Int -> Prim.String

instance Eq Int where
  eq = eq `on` _intToStr

instance Ord Int where
  compare = compare `on` \number ->
    -- Assuming every Int can be represented as BigInt
    unsafePartial $ fromJust $ BigInt.fromString $ _intToStr number

instance Show Int where
  show = _intToStr

fromBigInt :: BigInt.BigInt -> Maybe Int
fromBigInt bi =
  (newPositive <$> bigNumFromBigInt bi) <|>
  (newNegative <$> bigNumFromBigInt (negate bi))
