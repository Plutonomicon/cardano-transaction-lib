module Ctl.Internal.Deserialization.BigInt
  ( convertBigInt
  ) where

import Prelude

import Ctl.Internal.Serialization.Types (BigInt)
import Data.Maybe (Maybe)
import JS.BigInt as BigInt

convertBigInt :: BigInt -> Maybe BigInt.BigInt
convertBigInt = BigInt.fromString <<< _BigInt_to_str

foreign import _BigInt_to_str :: BigInt -> String
