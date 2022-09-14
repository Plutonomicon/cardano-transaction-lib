module CTL.Internal.Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashToBytes
  , ed25519KeyHashFromBytes
  , ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , scriptHashToBytes
  , scriptHashToBech32Unsafe
  , scriptHashFromBytes
  , scriptHashFromBech32
  , scriptHashToBech32
  , nativeScriptHash
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson'
  )
import CTL.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import CTL.Internal.FromData (class FromData)
import CTL.Internal.Metadata.FromMetadata (class FromMetadata)
import CTL.Internal.Metadata.ToMetadata (class ToMetadata, toMetadata)
import CTL.Internal.Serialization.Types (NativeScript)
import CTL.Internal.ToData (class ToData, toData)
import CTL.Internal.Types.Aliases (Bech32String)
import CTL.Internal.Types.PlutusData (PlutusData(Bytes))
import CTL.Internal.Types.RawBytes (RawBytes, hexToRawBytes, rawBytesToHex)
import CTL.Internal.Types.TransactionMetadata (TransactionMetadatum(Bytes)) as Metadata
import Data.Either (Either(Left, Right), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (unwrap, wrap)

-- | PubKeyHash and StakeKeyHash refers to blake2b-224 hash digests of Ed25519
-- | verification keys
foreign import data Ed25519KeyHash :: Type

instance Eq Ed25519KeyHash where
  eq = eq `on` ed25519KeyHashToBytes

instance Ord Ed25519KeyHash where
  compare = compare `on` ed25519KeyHashToBytes

instance Show Ed25519KeyHash where
  show edkh = "(Ed25519KeyHash " <> rawBytesToHex (ed25519KeyHashToBytes edkh)
    <> ")"

instance ToData Ed25519KeyHash where
  toData = toData <<< unwrap <<< ed25519KeyHashToBytes

instance FromData Ed25519KeyHash where
  fromData (Bytes kh) = ed25519KeyHashFromBytes $ wrap kh
  fromData _ = Nothing

instance ToMetadata Ed25519KeyHash where
  toMetadata = toMetadata <<< ed25519KeyHashToBytes

instance FromMetadata Ed25519KeyHash where
  fromMetadata (Metadata.Bytes kh) = ed25519KeyHashFromBytes $ wrap kh
  fromMetadata _ = Nothing

-- This is needed for `ApplyArgs`.
instance DecodeAeson Ed25519KeyHash where
  -- ed25519KeyHashFromBech32 goes from Bech32String directly although this
  -- feels unsafe.
  decodeAeson = caseAesonString
    (Left $ TypeMismatch "Expected Plutus BuiltinByteString")
    ( note (TypeMismatch "Invalid Ed25519KeyHash") <<< ed25519KeyHashFromBytes
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToRawBytes
    )

instance EncodeAeson Ed25519KeyHash where
  encodeAeson' = encodeAeson' <<< rawBytesToHex <<< ed25519KeyHashToBytes

foreign import _ed25519KeyHashFromBytesImpl
  :: MaybeFfiHelper
  -> RawBytes
  -> Maybe Ed25519KeyHash

foreign import _ed25519KeyHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe Ed25519KeyHash

foreign import ed25519KeyHashToBytes :: Ed25519KeyHash -> RawBytes

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import ed25519KeyHashToBech32Unsafe
  :: String
  -> Ed25519KeyHash
  -> Bech32String

foreign import _ed25519KeyHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> Ed25519KeyHash
  -> Maybe Bech32String

ed25519KeyHashFromBytes :: RawBytes -> Maybe Ed25519KeyHash
ed25519KeyHashFromBytes = _ed25519KeyHashFromBytesImpl maybeFfiHelper

ed25519KeyHashFromBech32 :: Bech32String -> Maybe Ed25519KeyHash
ed25519KeyHashFromBech32 = _ed25519KeyHashFromBech32Impl maybeFfiHelper

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
ed25519KeyHashToBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
ed25519KeyHashToBech32 = _ed25519KeyHashToBech32Impl maybeFfiHelper

-- | blake2b-224 hash digests of serialized monetary scripts
foreign import data ScriptHash :: Type

instance Eq ScriptHash where
  eq = eq `on` scriptHashToBytes

instance Ord ScriptHash where
  compare = compare `on` scriptHashToBytes

instance Show ScriptHash where
  show edkh = "(ScriptHash " <> rawBytesToHex (scriptHashToBytes edkh) <> ")"

instance ToData ScriptHash where
  toData = toData <<< unwrap <<< scriptHashToBytes

instance FromData ScriptHash where
  fromData (Bytes bytes) = scriptHashFromBytes $ wrap bytes
  fromData _ = Nothing

instance ToMetadata ScriptHash where
  toMetadata = toMetadata <<< scriptHashToBytes

instance FromMetadata ScriptHash where
  fromMetadata (Metadata.Bytes bytes) = scriptHashFromBytes $ wrap bytes
  fromMetadata _ = Nothing

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson ScriptHash where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded script hash") Right <<<
      caseAesonString Nothing (Just <=< scriptHashFromBytes <=< hexToRawBytes)

instance EncodeAeson ScriptHash where
  encodeAeson' sh = encodeAeson' $ scriptHashToBytes sh

foreign import _scriptHashFromBytesImpl
  :: MaybeFfiHelper
  -> RawBytes
  -> Maybe ScriptHash

foreign import _scriptHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe ScriptHash

-- | Encodes the hash to Cbor bytes
foreign import scriptHashToBytes :: ScriptHash -> RawBytes

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import scriptHashToBech32Unsafe
  :: String
  -> ScriptHash
  -> Bech32String

foreign import _scriptHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> ScriptHash
  -> Maybe Bech32String

-- | Decodes a script hash from its CBOR bytes encoding
-- | NOTE. It does _not_ compute hash of given bytes.
scriptHashFromBytes :: RawBytes -> Maybe ScriptHash
scriptHashFromBytes = _scriptHashFromBytesImpl maybeFfiHelper

-- | Decodes a script hash from its Bech32 representation
scriptHashFromBech32 :: Bech32String -> Maybe ScriptHash
scriptHashFromBech32 = _scriptHashFromBech32Impl maybeFfiHelper

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will return `Nothing` if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
scriptHashToBech32 :: String -> ScriptHash -> Maybe Bech32String
scriptHashToBech32 = _scriptHashToBech32Impl maybeFfiHelper

foreign import nativeScriptHash :: NativeScript -> ScriptHash
