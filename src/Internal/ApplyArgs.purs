module Ctl.Internal.ApplyArgs
  ( ApplyArgsError(..)
  , applyArgs
  , applyArgsWithErrors
  )
  where

import Aeson (class DecodeAeson, class EncodeAeson)
import Contract.Prelude (Either(..), Maybe(..), bind, note, ($))
import Ctl.Internal.Deserialization.WitnessSet as D
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as S
import Ctl.Internal.Serialization.PlutusScript (convertPlutusScript) as S
import Ctl.Internal.Serialization.Types as CSL
import Ctl.Internal.Types.PlutusData (PlutusData(List))
import Ctl.Internal.Types.Scripts (PlutusScript)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor.Choice (left)
import Data.Show.Generic (genericShow)
import Prelude (class Show)

foreign import apply_params_to_script :: CSL.PlutusData -> CSL.PlutusScript -> CSL.PlutusScript
-- foreign import apply_params_to_script_with_errors ::  CSL.PlutusData -> CSL.PlutusScript -> CSL.PlutusScript
foreign import apply_params_to_script_with_errors 
  :: (forall x. x -> Either x CSL.PlutusScript) -> (forall x. x -> Either String x)
  -> CSL.PlutusData -> CSL.PlutusScript -> Either String CSL.PlutusScript

apply_params_to_script_either :: CSL.PlutusData -> CSL.PlutusScript -> Either String CSL.PlutusScript
apply_params_to_script_either = apply_params_to_script_with_errors Left Right

applyArgs :: PlutusScript -> Array PlutusData -> Maybe PlutusScript
applyArgs script args =
  case (S.convertPlutusData (List args)) of
    Nothing -> Nothing
    Just args1 -> D.convertPlutusScript $ apply_params_to_script args1 (S.convertPlutusScript script)

newtype ApplyArgsError = ApplyArgsError String

derive instance Newtype ApplyArgsError _
derive instance Generic ApplyArgsError _
derive newtype instance EncodeAeson ApplyArgsError
derive newtype instance DecodeAeson ApplyArgsError

instance Show ApplyArgsError where
  show = genericShow

applyArgsWithErrors  :: PlutusScript -> Array PlutusData -> Either ApplyArgsError PlutusScript
applyArgsWithErrors script paramsList = left ApplyArgsError do
  params <- note "Error converting to serialized PlutusData" $ S.convertPlutusData (List paramsList)
  appliedScript <- apply_params_to_script_either params (S.convertPlutusScript script)
  note "Error converting back applied script" $ D.convertPlutusScript $ appliedScript