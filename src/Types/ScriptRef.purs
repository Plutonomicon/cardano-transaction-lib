module Cardano.Types.ScriptRef where

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
import Cardano.Serialization.Lib
  ( fromBytes
  , scriptRef_nativeScript
  , scriptRef_newNativeScript
  , scriptRef_newPlutusScript
  , scriptRef_plutusScript
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AsCbor (class AsCbor)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Control.Alt ((<|>))
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Types.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  )
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data ScriptRef = NativeScriptRef NativeScript | PlutusScriptRef PlutusScript

scriptRefFromMintingPolicy :: MintingPolicy -> ScriptRef
scriptRefFromMintingPolicy = case _ of
  PlutusMintingPolicy ps -> PlutusScriptRef ps
  NativeMintingPolicy ns -> NativeScriptRef ns

derive instance Eq ScriptRef
derive instance Generic ScriptRef _

instance Show ScriptRef where
  show = genericShow

instance AsCbor ScriptRef where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

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

toCsl :: ScriptRef -> Csl.ScriptRef
toCsl = case _ of
  NativeScriptRef ns -> scriptRef_newNativeScript $ NativeScript.toCsl ns
  PlutusScriptRef ps -> scriptRef_newPlutusScript $ PlutusScript.toCsl ps

fromCsl :: Csl.ScriptRef -> ScriptRef
fromCsl sr = unsafePartial $ fromJust $
  ( NativeScriptRef <<< NativeScript.fromCsl <$> toMaybe
      (scriptRef_nativeScript sr)
  ) <|>
    ( PlutusScriptRef <<< PlutusScript.fromCsl <$> toMaybe
        (scriptRef_plutusScript sr)
    )
