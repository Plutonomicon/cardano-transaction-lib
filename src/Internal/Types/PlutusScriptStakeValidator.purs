module Ctl.Internal.Types.PlutusScriptStakeValidator
  ( PlutusScriptStakeValidator(PlutusScriptStakeValidator)
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, encodeAeson)
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Ctl.Internal.Helpers (decodeTaggedNewtype)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | `PlutusScriptStakeValidator`s are used as validators for withdrawals and
-- | stake address certificates.
newtype PlutusScriptStakeValidator = PlutusScriptStakeValidator PlutusScript

derive instance Newtype PlutusScriptStakeValidator _
derive instance Generic PlutusScriptStakeValidator _
derive instance Eq PlutusScriptStakeValidator

instance DecodeAeson PlutusScriptStakeValidator where
  decodeAeson = decodeTaggedNewtype "getStakeValidator"
    PlutusScriptStakeValidator

instance EncodeAeson PlutusScriptStakeValidator where
  encodeAeson (PlutusScriptStakeValidator script) =
    encodeAeson { "getStakeValidator": script }

instance Show PlutusScriptStakeValidator where
  show = genericShow
