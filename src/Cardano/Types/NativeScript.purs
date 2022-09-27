module Cardano.Types.NativeScript
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
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson'
  , (.:)
  )
import Aeson.Decode.Decoders (decodeArray)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Helpers (encodeTagged')
import Metadata.Helpers (errExpectedObject)
import Serialization.Address (Slot)
import Serialization.Hash (Ed25519KeyHash)

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

instance DecodeAeson NativeScript where
  decodeAeson = caseAesonObject errExpectedObject $ \obj -> do
    tag <- obj .: "tag"
    let aesonContents = obj .: "contents"
    case tag of
      "ScriptPubkey" -> ScriptPubkey <$> (decodeAeson =<< aesonContents)
      "ScriptAll" -> ScriptAll <$>
        (decodeArray decodeAeson =<< (decodeAeson =<< aesonContents))
      "ScriptAny" -> ScriptAny <$>
        (decodeArray decodeAeson =<< (decodeAeson =<< aesonContents))
      "TimelockStart" -> TimelockStart <$> (decodeAeson =<< aesonContents)
      "TimelockExpiry" -> TimelockExpiry <$> (decodeAeson =<< aesonContents)
      "ScriptNOfK" -> aesonContents >>=
        caseAesonObject errExpectedObject
          ( \nOfkObj -> do
              n <- nOfkObj .: "n"
              scriptsAeson <- nOfkObj .: "nativeScripts"
              scripts <- decodeArray decodeAeson scriptsAeson
              pure $ ScriptNOfK n scripts
          )
      _ -> Left $ TypeMismatch ("Unknown tag" <> tag)

instance EncodeAeson NativeScript where
  encodeAeson' = case _ of
    ScriptPubkey r -> encodeAeson' $ encodeTagged' "ScriptPubkey" r
    ScriptAll r -> encodeAeson' $ encodeTagged' "ScriptAll" r
    ScriptAny r -> encodeAeson' $ encodeTagged' "ScriptAny" r
    ScriptNOfK n nativeScripts -> encodeAeson' $ encodeTagged' "ScriptNOfK"
      { n, nativeScripts }
    TimelockStart r -> encodeAeson' $ encodeTagged' "TimelockStart" r
    TimelockExpiry r -> encodeAeson' $ encodeTagged' "TimelockExpiry" r
