module Deserialization.BigNum
  ( bigNumToBigInt
  ) where

import Prelude

import Data.BigInt (BigInt, fromString)
import Data.Maybe (Maybe)
import Serialization.Types (BigNum)

foreign import bigNumToString :: BigNum -> String

bigNumToBigInt :: BigNum -> Maybe BigInt
bigNumToBigInt = bigNumToString >>> fromString
