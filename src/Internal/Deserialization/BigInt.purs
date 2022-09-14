module CTL.Internal.Deserialization.BigInt
  ( convertBigInt
  ) where

import Prelude

import Data.BigInt as BigInt
import Data.Maybe (Maybe)
import CTL.Internal.Serialization.Types (BigInt)

convertBigInt :: BigInt -> Maybe BigInt.BigInt
convertBigInt = BigInt.fromString <<< _BigInt_to_str

foreign import _BigInt_to_str :: BigInt -> String
