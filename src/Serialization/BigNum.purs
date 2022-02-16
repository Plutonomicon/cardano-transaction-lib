module Serialization.BigNum
  ( bigNumFromBigInt
  ) where

import Prelude
import Serialization.Types (BigNum)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Data.BigInt as BigInt
import Data.BigInt (BigInt)
import Data.Maybe (Maybe)

bigNumFromBigInt :: BigInt -> Maybe BigNum
bigNumFromBigInt = bigNumFromString maybeFfiHelper <<< BigInt.toString

foreign import bigNumFromString :: MaybeFfiHelper -> String -> Maybe BigNum
