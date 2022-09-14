module CTL.Internal.Cardano.Types.NativeScript
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
  ( class EncodeAeson
  , encodeAeson'
  )

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import CTL.Internal.Serialization.Address (Slot)
import CTL.Internal.Serialization.Hash (Ed25519KeyHash)
import CTL.Internal.Helpers (encodeTagged')

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
    ScriptPubkey r -> encodeAeson' $ encodeTagged' "ScriptPubKey" r
    ScriptAll r -> encodeAeson' $ encodeTagged' "ScriptAll" r
    ScriptAny r -> encodeAeson' $ encodeTagged' "ScriptAny" r
    ScriptNOfK n nativeScripts -> encodeAeson' $ encodeTagged' "ScriptPubKey"
      { n, nativeScripts }
    TimelockStart r -> encodeAeson' $ encodeTagged' "TimeLockStart" r
    TimelockExpiry r -> encodeAeson' $ encodeTagged' "TimeLockExpiry" r
