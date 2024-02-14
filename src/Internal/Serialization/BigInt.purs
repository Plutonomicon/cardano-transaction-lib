module Ctl.Internal.Serialization.BigInt
  ( convertBigInt
  ) where

import Prelude

import Cardano.Serialization.Lib (bigInt_fromStr)
import Ctl.Internal.Serialization.Types (BigInt)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)

convertBigInt :: BigInt.BigInt -> BigInt
convertBigInt bi = unsafePartial $ fromJust $ toMaybe $ bigInt_fromStr $
  BigInt.toString bi
