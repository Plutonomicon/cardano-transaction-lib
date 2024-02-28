module Ctl.Internal.Serialization.PlutusScript
  ( plutusScriptBytes
  , convertPlutusScript
  ) where

import Prelude

import Ctl.Internal.Serialization.Types (PlutusScript)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2, PlutusV3)
  , PlutusScript(PlutusScript)
  ) as T
import Data.Tuple.Nested ((/\))

foreign import plutusScriptBytes :: PlutusScript -> ByteArray

foreign import newPlutusV1Script :: ByteArray -> PlutusScript

foreign import newPlutusV2Script :: ByteArray -> PlutusScript

foreign import newPlutusV3Script :: ByteArray -> PlutusScript

convertPlutusScript :: T.PlutusScript -> PlutusScript
convertPlutusScript (T.PlutusScript (bytes /\ language)) =
  bytes # case language of
    T.PlutusV1 -> newPlutusV1Script
    T.PlutusV2 -> newPlutusV2Script
    T.PlutusV3 -> newPlutusV3Script
