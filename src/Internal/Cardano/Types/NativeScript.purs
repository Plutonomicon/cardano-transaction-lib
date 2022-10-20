module Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , encodeAeson'
  , fromString
  , toStringifiedNumbersJson
  , (.:)
  )
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data NativeScript
  = ScriptPubkey Ed25519KeyHash
  | ScriptAll (Array NativeScript)
  | ScriptAny (Array NativeScript)
  | ScriptNOfK Int (Array NativeScript)
  | TimelockStart Slot
  | TimelockExpiry Slot

derive instance Eq NativeScript
derive instance Generic NativeScript _

instance Show NativeScript where
  show x = genericShow x

instance EncodeAeson NativeScript where
  encodeAeson' = case _ of
    ScriptPubkey r -> encodeAeson' $ encodeTagged' "ScriptPubkey" r
    ScriptAll r -> encodeAeson' $ encodeTagged' "ScriptAll" r
    ScriptAny r -> encodeAeson' $ encodeTagged' "ScriptAny" r
    ScriptNOfK n nativeScripts -> encodeAeson' $ encodeTagged' "ScriptNOfK"
      { n, nativeScripts }
    TimelockStart r -> encodeAeson' $ encodeTagged' "TimelockStart" r
    TimelockExpiry r -> encodeAeson' $ encodeTagged' "TimelockExpiry" r

instance DecodeAeson NativeScript where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "ScriptPubkey" -> do
          pubKey <- obj .: "contents"
          pure $ ScriptPubkey pubKey
        "ScriptAll" -> do
          scripts <- obj .: "contents"
          pure $ ScriptAll scripts
        "ScriptAny" -> do
          scripts <- obj .: "contents"
          pure $ ScriptAny scripts
        "ScriptNOfK" -> do
          contents <- obj .: "contents"
          n <- contents .: "n"
          nativeScripts <- contents .: "nativeScripts"
          pure $ ScriptNOfK n nativeScripts
        "TimelockStart" -> do
          slot <- obj .: "contents"
          pure $ TimelockStart slot
        "TimelockExpiry" -> do
          slot <- obj .: "contents"
          pure $ TimelockExpiry slot
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
            tagValue
