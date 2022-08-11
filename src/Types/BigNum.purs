module Types.BigNum
  ( BigNum
  , add
  , fromBigInt
  , fromInt
  , fromString
  , fromStringUnsafe
  , maxValue
  , mul
  , one
  , toBigInt
  , toBigInt'
  , toBigIntUnsafe
  , toInt
  , toInt'
  , toString
  , fromUInt
  , toUInt
  , zero
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson')
import Aeson (JsonDecodeError(TypeMismatch)) as Aeson
import Data.BigInt (BigInt)
import Data.BigInt (fromString, toString) as BigInt
import Data.Either (note)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe, fromJust)
import Data.UInt (fromInt, fromString, toString) as UInt
import Data.UInt (UInt)
import Deserialization.Error (FromCslRepError, fromCslRepError)
import Error (E, noteE)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))

foreign import data BigNum :: Type

instance Eq BigNum where
  eq lhs rhs = bnCompare lhs rhs == 0

instance Ord BigNum where
  compare lhs rhs =
    case bnCompare lhs rhs of
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
  encodeAeson' = encodeAeson' <<< toBigIntUnsafe

fromBigInt :: BigInt -> Maybe BigNum
fromBigInt = fromString <<< BigInt.toString

toBigInt :: BigNum -> Maybe BigInt
toBigInt = BigInt.fromString <<< toString

toBigIntUnsafe :: BigNum -> BigInt
toBigIntUnsafe =
  -- Converting uint64 to an arbitrary length integer should never fail.
  unsafePartial fromJust <<< toBigInt

toBigInt'
  :: forall (r :: Row Type). String -> BigNum -> E (FromCslRepError + r) BigInt
toBigInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> BigInt "))
    $ toBigInt bn

toInt :: BigNum -> Maybe Int
toInt = Int.fromString <<< toString

toInt'
  :: forall (r :: Row Type). String -> BigNum -> E (FromCslRepError + r) Int
toInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> Int ")) $
    toInt bn

foreign import bnCompare :: BigNum -> BigNum -> Int

foreign import zero :: BigNum

foreign import one :: BigNum

foreign import bnAdd :: MaybeFfiHelper -> BigNum -> BigNum -> Maybe BigNum

add :: BigNum -> BigNum -> Maybe BigNum
add = bnAdd maybeFfiHelper

foreign import bnMul :: MaybeFfiHelper -> BigNum -> BigNum -> Maybe BigNum

mul :: BigNum -> BigNum -> Maybe BigNum
mul = bnMul maybeFfiHelper

-- | Converts an `Int` to a `BigNum` turning negative `Int`s into `BigNum`s
-- | in range from `2^31` to `2^32-1`.
fromInt :: Int -> BigNum
fromInt =
  -- Converting `UInt` (u32) to a `BigNum` (u64) should never fail.
  fromStringUnsafe <<< UInt.toString <<< UInt.fromInt

foreign import _fromString :: MaybeFfiHelper -> String -> Maybe BigNum

fromString :: String -> Maybe BigNum
fromString = _fromString maybeFfiHelper

fromStringUnsafe :: String -> BigNum
fromStringUnsafe = unsafePartial fromJust <<< fromString

foreign import toString :: BigNum -> String

maxValue :: BigNum
maxValue = fromStringUnsafe "18446744073709551615"

fromUInt :: UInt -> BigNum
fromUInt = fromStringUnsafe <<< UInt.toString

toUInt :: BigNum -> Maybe UInt
toUInt = toString >>> UInt.fromString
