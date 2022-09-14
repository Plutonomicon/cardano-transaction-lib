module CTL.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  , getNativeScript
  , getPlutusScript
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , encodeAeson'
  )
import CTL.Internal.Cardano.Types.NativeScript (NativeScript)
import CTL.Internal.Helpers (encodeTagged')
import CTL.Internal.Types.Scripts (PlutusScript)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

data ScriptRef = NativeScriptRef NativeScript | PlutusScriptRef PlutusScript

derive instance Eq ScriptRef
derive instance Generic ScriptRef _

instance Show ScriptRef where
  show = genericShow

instance EncodeAeson ScriptRef where
  encodeAeson' = case _ of
    NativeScriptRef r -> encodeAeson' $ encodeTagged' "NativeScriptRef" r
    PlutusScriptRef r -> encodeAeson' $ encodeTagged' "PlutusScriptRef" r

getNativeScript :: ScriptRef -> Maybe NativeScript
getNativeScript (NativeScriptRef nativeScript) = Just nativeScript
getNativeScript _ = Nothing

getPlutusScript :: ScriptRef -> Maybe PlutusScript
getPlutusScript (PlutusScriptRef plutusScript) = Just plutusScript
getPlutusScript _ = Nothing
