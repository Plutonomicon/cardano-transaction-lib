module Types.BigNum
  ( BigNum
  , add
  , fromBigInt
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

-- Converting uint64 to an arbitrary length integer should never fail.
toBigIntUnsafe :: BigNum -> BigInt
toBigIntUnsafe bn = unsafePartial fromJust $ toBigInt bn

toBigInt' :: forall r. String -> BigNum -> E (FromCslRepError + r) BigInt
toBigInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> BigInt "))
    $ toBigInt bn

toInt :: BigNum -> Maybe Int
toInt = Int.fromString <<< toString

toInt' :: forall r. String -> BigNum -> E (FromCslRepError + r) Int
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

foreign import _fromString :: MaybeFfiHelper -> String -> Maybe BigNum

fromString :: String -> Maybe BigNum
fromString = _fromString maybeFfiHelper

fromStringUnsafe :: String -> BigNum
fromStringUnsafe str = unsafePartial fromJust $ fromString str

foreign import toString :: BigNum -> String

maxValue :: BigNum
maxValue = fromStringUnsafe "18446744073709551615"
