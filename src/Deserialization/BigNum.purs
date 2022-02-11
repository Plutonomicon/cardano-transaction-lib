module Deserialization.BigNum
  ( convertBigNum
  ) where

import Prelude

import Data.BigInt (BigInt, fromString)
import Data.Maybe (Maybe)
import Serialization.Types (BigNum)

foreign import bigNumToString :: BigNum -> String

convertBigNum :: BigNum -> Maybe BigInt
convertBigNum = bigNumToString >>> fromString
