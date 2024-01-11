module Ctl.Internal.Serialization.PlutusScript
  ( convertPlutusScript
  ) where

import Prelude

import Cardano.Serialization.Lib (plutusScript_new, plutusScript_newV2)
import Ctl.Internal.Serialization.Types (PlutusScript)
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , PlutusScript(PlutusScript)
  ) as T
import Data.Tuple.Nested ((/\))

convertPlutusScript :: T.PlutusScript -> PlutusScript
convertPlutusScript (T.PlutusScript (bytes /\ language)) =
  bytes # case language of
    T.PlutusV1 -> plutusScript_new
    T.PlutusV2 -> plutusScript_newV2
