module Deserialization.BigNum
  ( bigNumToBigInt
  , bigNumToBigInt'
  , bigNumToInt
  , bigNumToInt'
  ) where

import Prelude

import Data.BigInt (BigInt, fromString)
import Data.Int as Int
import Data.Maybe (Maybe)
import Deserialization.Error (FromCslRepError, fromCslRepError)
import Error (E, noteE)
import Serialization.Types (BigNum)
import Type.Row (type (+))

foreign import bigNumToString :: BigNum -> String

bigNumToBigInt :: BigNum -> Maybe BigInt
bigNumToBigInt = bigNumToString >>> fromString

bigNumToBigInt' :: forall r. String -> BigNum -> E (FromCslRepError + r) BigInt
bigNumToBigInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> BigInt "))
    $ bigNumToBigInt bn

bigNumToInt :: BigNum -> Maybe Int
bigNumToInt = bigNumToString >>> Int.fromString

bigNumToInt' :: forall r. String -> BigNum -> E (FromCslRepError + r) Int
bigNumToInt' nm bn =
  noteE (fromCslRepError (nm <> ": CSL.BigNum (" <> show bn <> ") -> Int ")) $
    bigNumToInt bn
