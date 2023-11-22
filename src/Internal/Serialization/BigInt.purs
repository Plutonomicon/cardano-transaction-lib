module Ctl.Internal.Serialization.BigInt
  ( convertBigInt
  ) where

import Prelude

import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types (BigInt)
import Data.Maybe (Maybe)
import JS.BigInt as BigInt

convertBigInt :: BigInt.BigInt -> Maybe BigInt
convertBigInt = _BigInt_from_str maybeFfiHelper <<< BigInt.toString

foreign import _BigInt_from_str :: MaybeFfiHelper -> String -> Maybe BigInt
