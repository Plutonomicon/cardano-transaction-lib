-- | `cardano-serialization-lib` Int type (can be positive or negative).
module Ctl.Internal.Types.Int
  ( Int
  , newPositive
  , newNegative
  , fromBigInt
  , toBigInt
  , fromInt
  , toInt
  , fromString
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Control.Alternative ((<|>))
import Ctl.Internal.Types.BigNum (BigNum)
import Ctl.Internal.Types.BigNum (fromBigInt, fromInt) as BigNum
import Data.Either (note)
import Data.Function (on)
import Data.Maybe (Maybe, fromJust)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Prim as Prim

foreign import data Int :: Prim.Type

foreign import newPositive :: BigNum -> Int
foreign import newNegative :: BigNum -> Int
foreign import _intToStr :: Int -> Prim.String

instance Eq Int where
  eq = eq `on` _intToStr

instance Ord Int where
  compare = compare `on` toBigInt

instance Show Int where
  show = _intToStr

instance EncodeAeson Int where
  encodeAeson = encodeAeson <<< toBigInt

instance DecodeAeson Int where
  decodeAeson aeson =
    decodeAeson aeson >>= note (TypeMismatch "Int") <<< fromBigInt

fromBigInt :: BigInt.BigInt -> Maybe Int
fromBigInt bi =
  (newPositive <$> BigNum.fromBigInt bi) <|>
    (newNegative <$> BigNum.fromBigInt (negate bi))

toBigInt :: Int -> BigInt.BigInt
toBigInt int =
  -- Assuming every Int can be represented as BigInt
  unsafePartial $ fromJust $ BigInt.fromString $ _intToStr int

fromInt :: Prim.Int -> Int
fromInt n
  | n < 0 = newNegative $ BigNum.fromInt n
  | otherwise = newPositive $ BigNum.fromInt n

toInt :: Int -> Maybe Prim.Int
toInt = toBigInt >>> BigInt.toInt

fromString :: Prim.String -> Maybe Int
fromString = fromBigInt <=< BigInt.fromString
