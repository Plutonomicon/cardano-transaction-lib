module Serialization.BigInt
  ( convertBigInt
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.Maybe (Maybe)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Types (BigInt)

convertBigInt :: BigInt.BigInt -> Maybe BigInt
convertBigInt = _BigInt_from_str maybeFfiHelper <<< BigInt.toString

foreign import _BigInt_from_str :: MaybeFfiHelper -> String -> Maybe BigInt
