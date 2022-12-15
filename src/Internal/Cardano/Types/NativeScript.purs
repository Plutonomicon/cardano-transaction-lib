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
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , getField
  , (.:)
  )
import Ctl.Internal.Helpers (contentsProp, encodeTagged', tagProp)
import Ctl.Internal.Metadata.Helpers (errExpectedObject)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Serialization.Hash (Ed25519KeyHash, ed25519KeyHashFromBytes)
import Ctl.Internal.Types.BigNum (fromString)
import Ctl.Internal.Types.RawBytes (hexToRawBytesUnsafe)
import Data.Array.NonEmpty (fromFoldable)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf, resize, sized, suchThat)

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

instance Arbitrary NativeScript where
  arbitrary = oneOf $ unsafePartial $ fromJust $ fromFoldable
    [ ScriptPubkey <$> (pure pk)
    , ScriptAll <$> sized (\i -> resize (i `div` 2) arbitrary)
    , ScriptAny <$> sized (\i -> resize (i `div` 2) arbitrary)
    , ScriptNOfK <$> arbitrary <*> sized (\i -> resize (i `div` 2) arbitrary)
    , TimelockStart <$> map
        (wrap <<< (unsafePartial $ fromJust <<< fromString <<< show))
        (suchThat (arbitrary :: Gen Int) (_ > 0))
    , TimelockExpiry <$> map
        (wrap <<< (unsafePartial $ fromJust <<< fromString <<< show))
        (suchThat (arbitrary :: Gen Int) (_ > 0))
    ]
    where
    pk :: Ed25519KeyHash
    pk = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $
      hexToRawBytesUnsafe
        "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

instance DecodeAeson NativeScript where
  decodeAeson = caseAesonObject errExpectedObject $ \obj -> do
    tag <- obj .: tagProp
    let
      aesonContents
        :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: contentsProp
    case tag of
      "ScriptPubkey" -> ScriptPubkey <$> aesonContents
      "ScriptAll" -> ScriptAll <$> aesonContents
      "ScriptAny" -> ScriptAny <$> aesonContents
      "TimelockStart" -> TimelockStart <$> aesonContents
      "TimelockExpiry" -> TimelockExpiry <$> aesonContents
      "ScriptNOfK" -> ScriptNOfK
        <$> (flip getField "n" =<< aesonContents)
        <*> (flip getField "nativeScripts" =<< aesonContents)

      _ -> Left $ TypeMismatch ("Unknown tag: " <> tag)

instance EncodeAeson NativeScript where
  encodeAeson = case _ of
    ScriptPubkey r -> encodeTagged' "ScriptPubkey" r
    ScriptAll r -> encodeTagged' "ScriptAll" r
    ScriptAny r -> encodeTagged' "ScriptAny" r
    ScriptNOfK n nativeScripts -> encodeTagged' "ScriptNOfK"
      { n, nativeScripts }
    TimelockStart r -> encodeTagged' "TimelockStart" r
    TimelockExpiry r -> encodeTagged' "TimelockExpiry" r
