module Ctl.Internal.ApplyArgs
  ( applyArgs
  )
  where

import Contract.Prelude (Maybe(..), bind, pure, ($))
import Ctl.Internal.Serialization.PlutusData as S
import Ctl.Internal.Serialization.PlutusScript as S
import Ctl.Internal.Deserialization.WitnessSet as D
import Ctl.Internal.Serialization.Types as CSL
import Ctl.Internal.Types.PlutusData (PlutusData(List))
import Ctl.Internal.Types.Scripts (PlutusScript(PlutusScript))
import Data.ArrayBuffer.Types (Uint8Array)

foreign import apply_params_to_script :: CSL.PlutusData -> CSL.PlutusScript -> CSL.PlutusScript

applyArgs :: PlutusScript -> Array PlutusData -> Maybe PlutusScript
applyArgs script args =
  case (S.convertPlutusData (List args)) of
    Nothing -> Nothing
    Just args1 -> D.convertPlutusScript $ apply_params_to_script args1 (S.convertPlutusScript script)