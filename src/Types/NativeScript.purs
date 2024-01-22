module Cardano.Types.NativeScript where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , getField
  , (.:)
  )
import Cardano.Serialization.Lib
  ( nativeScript_asScriptAll
  , nativeScript_asScriptAny
  , nativeScript_asScriptNOfK
  , nativeScript_asScriptPubkey
  , nativeScript_asTimelockExpiry
  , nativeScript_asTimelockStart
  , nativeScript_newScriptAll
  , nativeScript_newScriptAny
  , nativeScript_newScriptNOfK
  , nativeScript_newScriptPubkey
  , nativeScript_newTimelockExpiry
  , nativeScript_newTimelockStart
  , scriptAll_nativeScripts
  , scriptAll_new
  , scriptAny_nativeScripts
  , scriptAny_new
  , scriptNOfK_n
  , scriptNOfK_nativeScripts
  , scriptNOfK_new
  , scriptPubkey_addrKeyhash
  , scriptPubkey_new
  , timelockExpiry_newTimelockexpiry
  , timelockExpiry_slotBignum
  , timelockStart_newTimelockstart
  , timelockStart_slotBignum
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Serialization.Lib.Internal
  ( packListContainer
  , unpackListContainer
  )
import Cardano.Types.BigNum (fromString)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Slot (Slot(..))
import Control.Alt ((<|>))
import Ctl.Internal.Helpers (encodeTagged')
import Ctl.Internal.Metadata.Helpers (errExpectedObject)
import Ctl.Internal.Serialization.Address (Slot)
import Ctl.Internal.Serialization.Hash
  ( Ed25519KeyHash
  , ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  )
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Data.Array.NonEmpty (fromFoldable)
import Data.ByteArray (hexToByteArrayUnsafe)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
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
  | TimelockStart Slot -- spend after
  | TimelockExpiry Slot -- spend before

derive instance Eq NativeScript
derive instance Generic NativeScript _

instance Show NativeScript where
  show x = genericShow x

instance Arbitrary NativeScript where
  arbitrary = oneOf $ unsafePartial $ fromJust $ fromFoldable
    [ ScriptPubkey <$> (pure pk)
    , ScriptAll <$> sized (\i -> resize (i `div` 2) arbitrary)
    , ScriptAny <$> sized (\i -> resize (i `div` 2) arbitrary)
    , ScriptNOfK
        <$> suchThat (arbitrary :: Gen Int) (_ >= 0)
        <*> sized (\i -> resize (i `div` 2) arbitrary)
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
      hexToByteArrayUnsafe
        "1c12f03c1ef2e935acc35ec2e6f96c650fd3bfba3e96550504d53361"

instance DecodeAeson NativeScript where
  decodeAeson = caseAesonObject errExpectedObject $ \obj -> do
    tag <- obj .: "tag"
    let
      aesonContents
        :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: "contents"
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

pprintNativeScript :: NativeScript -> TagSet
pprintNativeScript = case _ of
  ScriptPubkey kh -> TagSet.fromArray
    [ "PubKey" `tag` rawBytesToHex (ed25519KeyHashToBytes kh) ]
  ScriptAll scripts -> "All of" `tagSetTag` TagSet.fromArray
    (pprintNativeScript <$> scripts)
  ScriptAny scripts -> "Any of" `tagSetTag` TagSet.fromArray
    (pprintNativeScript <$> scripts)
  ScriptNOfK n scripts -> ("At least " <> show n <> " of ")
    `tagSetTag` TagSet.fromArray (pprintNativeScript <$> scripts)
  TimelockStart slot -> "Timelock start" `tag` BigNum.toString (unwrap slot)
  TimelockExpiry slot -> "Timelock expiry" `tag` BigNum.toString (unwrap slot)

toCslMany :: Array NativeScript -> Csl.NativeScripts
toCslMany = packListContainer <<< map toCsl

fromCsl :: Csl.NativeScript -> NativeScript
fromCsl ns = unsafePartial $ fromJust $
  convertScriptPubkey <$> toMaybe (nativeScript_asScriptPubkey ns)
    <|> convertScriptAll <$> toMaybe (nativeScript_asScriptAll ns)
    <|> convertScriptAny <$> toMaybe (nativeScript_asScriptAny ns)
    <|> convertScriptNOfK <$> toMaybe (nativeScript_asScriptNOfK ns)
    <|> convertScriptTimelockStart <$> toMaybe (nativeScript_asTimelockStart ns)
    <|>
      convertScriptTimelockExpiry <$> toMaybe (nativeScript_asTimelockExpiry ns)
  where
  convertScriptPubkey = scriptPubkey_addrKeyhash >>> wrap >>> ScriptPubkey
  convertScriptAll =
    scriptAll_nativeScripts >>> unpackListContainer >>> map fromCsl >>>
      ScriptAll
  convertScriptAny =
    scriptAny_nativeScripts >>> unpackListContainer >>> map fromCsl >>>
      ScriptAny
  convertScriptNOfK nOfK = ScriptNOfK
    (unsafePartial $ fromJust $ Int.fromNumber $ scriptNOfK_n nOfK)
    (map fromCsl $ unpackListContainer $ scriptNOfK_nativeScripts nOfK)
  convertScriptTimelockStart =
    timelockStart_slotBignum >>> wrap >>> Slot >>> TimelockStart
  convertScriptTimelockExpiry =
    timelockExpiry_slotBignum >>> wrap >>> Slot >>> TimelockExpiry

-- | Note: unbounded recursion here.
toCsl :: NativeScript -> Csl.NativeScript
toCsl = case _ of
  ScriptPubkey keyHash -> convertScriptPubkey keyHash
  ScriptAll nss -> convertScriptAll nss
  ScriptAny nss -> convertScriptAny nss
  ScriptNOfK n nss -> convertScriptNOfK n nss
  TimelockStart slot -> convertTimelockStart slot
  TimelockExpiry slot -> convertTimelockExpiry slot
  where
  convertScriptPubkey :: Ed25519KeyHash -> Csl.NativeScript
  convertScriptPubkey hash = do
    nativeScript_newScriptPubkey $ scriptPubkey_new (unwrap hash)

  convertScriptAll :: Array NativeScript -> Csl.NativeScript
  convertScriptAll nss =
    nativeScript_newScriptAll <<< scriptAll_new <<<
      packListContainer $ map toCsl nss

  convertScriptAny :: Array NativeScript -> Csl.NativeScript
  convertScriptAny nss =
    nativeScript_newScriptAny <<< scriptAny_new <<<
      packListContainer $ map toCsl nss

  convertScriptNOfK :: Int -> Array NativeScript -> Csl.NativeScript
  convertScriptNOfK n nss =
    nativeScript_newScriptNOfK <<< scriptNOfK_new (Int.toNumber n) <<<
      packListContainer $ map toCsl nss

  convertTimelockStart :: Slot -> Csl.NativeScript
  convertTimelockStart (Slot slot) =
    nativeScript_newTimelockStart (timelockStart_newTimelockstart $ unwrap slot)

  convertTimelockExpiry :: Slot -> Csl.NativeScript
  convertTimelockExpiry (Slot slot) =
    nativeScript_newTimelockExpiry
      (timelockExpiry_newTimelockexpiry $ unwrap slot)
