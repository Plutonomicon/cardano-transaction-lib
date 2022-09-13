module Serialization.PlutusScript
  ( convertPlutusScript
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import Serialization.Types (PlutusScript)
import Types.ByteArray (ByteArray)
import Types.Scripts (Language(PlutusV1, PlutusV2), PlutusScript(PlutusScript)) as T

foreign import newPlutusV1Script :: ByteArray -> PlutusScript

foreign import newPlutusV2Script :: ByteArray -> PlutusScript

convertPlutusScript :: T.PlutusScript -> PlutusScript
convertPlutusScript (T.PlutusScript (bytes /\ language)) =
  bytes # case language of
    T.PlutusV1 -> newPlutusV1Script
    T.PlutusV2 -> newPlutusV2Script
