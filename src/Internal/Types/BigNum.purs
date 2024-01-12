module Ctl.Internal.Types.BigNum
  ( BigNum
  , add
  , divFloor
  , fromBigInt
  , fromInt
  , fromString
  , fromStringUnsafe
  , fromUInt
  , maxValue
  , mul
  , one
  , toBigInt
  , toInt
  , toInt'
  , toString
  , toUInt
  , zero
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Aeson (JsonDecodeError(TypeMismatch)) as Aeson
import Cardano.Serialization.Lib
  ( bigNum_checkedAdd
  , bigNum_checkedMul
  , bigNum_compare
  , bigNum_divFloor
  , bigNum_fromStr
  , bigNum_maxValue
  , bigNum_one
  , bigNum_toStr
  , bigNum_zero
  )
import Cardano.Serialization.Lib as Csl
import Ctl.Internal.Deserialization.Error (FromCslRepError, fromCslRepError)
import Ctl.Internal.Error (E, noteE)
import Data.Either (note)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.UInt (UInt)
import Data.UInt (fromInt, fromString, toString) as UInt
import JS.BigInt (BigInt)
import JS.BigInt (fromString, toString) as BigInt
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Type.Row (type (+))

newtype BigNum = BigNum Csl.BigNum

derive instance Newtype BigNum _

instance Eq BigNum where
  eq x y = compare x y == EQ

instance Ord BigNum where
  compare (BigNum lhs) (BigNum rhs) =
    case bigNum_compare lhs rhs of
      1 -> GT
      0 -> EQ
      _ -> LT

instance Show BigNum where
  show bn = "fromString \"" <> toString bn <> "\""

instance DecodeAeson BigNum where
  decodeAeson =
    note (Aeson.TypeMismatch "Couldn't convert `BigInt` to `BigNum`")
      <<< fromBigInt <=< decodeAeson

instance EncodeAeson BigNum where
  encodeAeson = encodeAeson <<< toBigInt

-- Semiring cannot be implemented, because add and mul returns Maybe BigNum

fromBigInt :: BigInt -> Maybe BigNum
fromBigInt = fromString <<< BigInt.toString

toBigInt :: BigNum -> BigInt
toBigInt =
  -- Converting uint64 to an arbitrary length integer should never fail.
  unsafePartial fromJust <<< BigInt.fromString <<< toString

toInt :: BigNum -> Maybe Int
toInt = Int.fromString <<< toString

toInt'
  :: forall (r :: Row Type). String -> BigNum -> E (FromCslRepError + r) Int
toInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> Int ")) $
    toInt bn

one :: BigNum
one = BigNum bigNum_one

zero :: BigNum
zero = BigNum bigNum_zero

add :: BigNum -> BigNum -> Maybe BigNum
add (BigNum a) (BigNum b) = coerce $ toMaybe $ bigNum_checkedAdd a b

mul :: BigNum -> BigNum -> Maybe BigNum
mul (BigNum a) (BigNum b) = coerce $ toMaybe $ bigNum_checkedMul a b

divFloor :: BigNum -> BigNum -> BigNum
divFloor (BigNum a) (BigNum b) = BigNum $ bigNum_divFloor a b

-- | Converts an `Int` to a `BigNum` turning negative `Int`s into `BigNum`s
-- | in range from `2^31` to `2^32-1`.
fromInt :: Int -> BigNum
fromInt =
  -- Converting `UInt` (u32) to a `BigNum` (u64) should never fail.
  fromStringUnsafe <<< UInt.toString <<< UInt.fromInt

toString :: BigNum -> String
toString = unwrap >>> bigNum_toStr

fromString :: String -> Maybe BigNum
fromString = map wrap <<< toMaybe <<< bigNum_fromStr

fromStringUnsafe :: String -> BigNum
fromStringUnsafe = unsafePartial fromJust <<< fromString

maxValue :: BigNum
maxValue = BigNum bigNum_maxValue

fromUInt :: UInt -> BigNum
fromUInt = fromStringUnsafe <<< UInt.toString

toUInt :: BigNum -> Maybe UInt
toUInt = toString >>> UInt.fromString
