module Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  , scriptRefFromMintingPolicy
  , getNativeScript
  , getPlutusScript
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue, TypeMismatch)
  , caseAesonObject
  , fromString
  , toStringifiedNumbersJson
  , (.:)
  )
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , PlutusScript
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)

data ScriptRef = NativeScriptRef NativeScript | PlutusScriptRef PlutusScript

scriptRefFromMintingPolicy :: MintingPolicy -> ScriptRef
scriptRefFromMintingPolicy = case _ of
  PlutusMintingPolicy ps -> PlutusScriptRef ps
  NativeMintingPolicy ns -> NativeScriptRef ns

derive instance Eq ScriptRef
derive instance Generic ScriptRef _

instance Show ScriptRef where
  show = genericShow

instance EncodeAeson ScriptRef where
  encodeAeson = case _ of
    NativeScriptRef r -> encodeTagged' "NativeScriptRef" r
    PlutusScriptRef r -> encodeTagged' "PlutusScriptRef" r

instance DecodeAeson ScriptRef where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "NativeScriptRef" -> do
          nativeScript <- obj .: "contents"
          pure $ NativeScriptRef nativeScript
        "PlutusScriptRef" -> do
          plutusScript <- obj .: "contents"
          pure $ PlutusScriptRef plutusScript
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
            tagValue

getNativeScript :: ScriptRef -> Maybe NativeScript
getNativeScript (NativeScriptRef nativeScript) = Just nativeScript
getNativeScript _ = Nothing

getPlutusScript :: ScriptRef -> Maybe PlutusScript
getPlutusScript (PlutusScriptRef plutusScript) = Just plutusScript
getPlutusScript _ = Nothing
