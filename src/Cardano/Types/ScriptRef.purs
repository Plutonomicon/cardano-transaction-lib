module Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , encodeAeson'
  )
import Helpers (encodeTagged')
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Cardano.Types.NativeScript (NativeScript)
import Types.Scripts (PlutusScript)

data ScriptRef = NativeScriptRef NativeScript | PlutusScriptRef PlutusScript

derive instance Eq ScriptRef
derive instance Generic ScriptRef _

instance Show ScriptRef where
  show = genericShow

instance EncodeAeson ScriptRef where
  encodeAeson' = case _ of
    NativeScriptRef r -> encodeAeson' $ encodeTagged' "NativeScriptRef" r
    PlutusScriptRef r -> encodeAeson' $ encodeTagged' "PlutusScriptRef" r
