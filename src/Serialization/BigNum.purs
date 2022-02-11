module Serialization.BigNum
  ( convertBigNum
  ) where

import Prelude
import Serialization.Types (BigNum)
import FfiHelpers
import Data.BigInt as BigInt
import Data.BigInt (BigInt)
import Data.Maybe

convertBigNum :: BigInt -> Maybe BigNum
convertBigNum = bigNumFromString maybeFfiHelper <<< BigInt.toString

foreign import bigNumFromString :: MaybeFfiHelper -> String -> Maybe BigNum
