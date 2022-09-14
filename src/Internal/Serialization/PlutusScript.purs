module CTL.Internal.Serialization.PlutusScript
  ( convertPlutusScript
  ) where

import Prelude

import CTL.Internal.Serialization.Types (PlutusScript)
import CTL.Internal.Types.ByteArray (ByteArray)
import CTL.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , PlutusScript(PlutusScript)
  ) as T
import Data.Tuple.Nested ((/\))

foreign import newPlutusV1Script :: ByteArray -> PlutusScript

foreign import newPlutusV2Script :: ByteArray -> PlutusScript

convertPlutusScript :: T.PlutusScript -> PlutusScript
convertPlutusScript (T.PlutusScript (bytes /\ language)) =
  bytes # case language of
    T.PlutusV1 -> newPlutusV1Script
    T.PlutusV2 -> newPlutusV2Script
