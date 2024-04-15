module Ctl.Internal.ApplyArgs
  ( applyArgs
  ) where

import Prelude

import Cardano.Serialization.Lib as CSL
import Cardano.Types.PlutusData (PlutusData(List))
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Data.Either (Either(Left, Right))

foreign import apply_params_to_script
  :: (forall (x :: Type). x -> Either x CSL.PlutusScript)
  -> (forall (x :: Type). x -> Either String x)
  -> CSL.PlutusData
  -> CSL.PlutusScript
  -> Either String CSL.PlutusScript

apply_params_to_script_either
  :: CSL.PlutusData -> CSL.PlutusScript -> Either String CSL.PlutusScript
apply_params_to_script_either = apply_params_to_script Left Right

applyArgs
  :: PlutusScript -> Array PlutusData -> Either String PlutusScript
applyArgs script paramsList = do
  let params = PlutusData.toCsl (List paramsList)
  appliedScript <- apply_params_to_script_either params
    (PlutusScript.toCsl script)
  Right $ PlutusScript.fromCsl appliedScript
