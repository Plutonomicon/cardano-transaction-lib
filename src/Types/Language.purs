module Cardano.Types.Language where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , fromString
  , getField
  , toStringifiedNumbersJson
  )
import Cardano.Serialization.Lib
  ( language_kind
  , language_newPlutusV1
  , language_newPlutusV2
  )
import Cardano.Serialization.Lib as Csl
import Control.Alt ((<|>))
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.Helpers (notImplemented)
import Ctl.Internal.Metadata.FromMetadata (class FromMetadata)
import Ctl.Internal.Metadata.ToMetadata (class ToMetadata)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

data Language
  = PlutusV1
  | PlutusV2

derive instance Eq Language
derive instance Ord Language
derive instance Generic Language _

instance DecodeAeson Language where
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected string")
    case _ of
      "PlutusV1" -> pure PlutusV1
      "PlutusV2" -> pure PlutusV2
      other -> Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
        other

instance EncodeAeson Language where
  encodeAeson = encodeAeson <<< case _ of
    PlutusV1 -> "PlutusV1"
    PlutusV2 -> "PlutusV2"

instance Show Language where
  show = genericShow

fromCsl :: Csl.Language -> Language
fromCsl lang =
  case language_kind lang of
    0.0 -> PlutusV1
    1.1 -> PlutusV2
    _ -> unsafePerformEffect $ throw
      "Cardano.Types.Language.fromCsl: unknown kind"

toCsl :: Language -> Csl.Language
toCsl PlutusV1 = language_newPlutusV1
toCsl PlutusV2 = language_newPlutusV2
