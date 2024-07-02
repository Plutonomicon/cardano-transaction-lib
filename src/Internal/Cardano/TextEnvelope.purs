module Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PlutusScriptV3
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      , Other
      )
  , decodeTextEnvelope
  , plutusScriptFromEnvelope
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Cardano.Types.Language (Language(PlutusV3))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Ctl.Internal.Types.Cbor (toByteArray)
import Data.ByteArray (ByteArray, hexToByteArray)
import Data.Either (hush)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))

data TextEnvelopeType
  = PlutusScriptV1
  | PlutusScriptV2
  | PlutusScriptV3
  | PaymentSigningKeyShelleyed25519
  | StakeSigningKeyShelleyed25519
  | Other String

derive instance Eq TextEnvelopeType

instance Show TextEnvelopeType where
  show = case _ of
    PlutusScriptV1 -> "PlutusScriptV1"
    PlutusScriptV2 -> "PlutusScriptV2"
    PlutusScriptV3 -> "PlutusScriptV3"
    PaymentSigningKeyShelleyed25519 -> "PaymentSigningKeyShelley_ed25519"
    StakeSigningKeyShelleyed25519 -> "StakeSigningKeyShelley_ed25519"
    Other other -> other

instance DecodeAeson TextEnvelopeType where
  decodeAeson aeson = do
    decodeAeson aeson >>= case _ of
      "PlutusScriptV1" -> pure PlutusScriptV1
      "PlutusScriptV2" -> pure PlutusScriptV2
      "PlutusScriptV3" -> pure PlutusScriptV3
      "PaymentSigningKeyShelley_ed25519" -> pure
        PaymentSigningKeyShelleyed25519
      "StakeSigningKeyShelley_ed25519" -> pure
        StakeSigningKeyShelleyed25519
      other -> pure $ Other other

type TextEnvelopeRaw =
  { "type" :: TextEnvelopeType
  , description :: String
  , cborHex :: String
  }

-- | We only support `TextEnvelope`s whose cborHex represents the cbor type byte string
newtype TextEnvelope =
  TextEnvelope
    { type_ :: TextEnvelopeType
    , description :: String
    , bytes :: ByteArray
    }

derive instance Newtype TextEnvelope _

decodeCborHexToBytes :: String -> Maybe ByteArray
decodeCborHexToBytes cborHex = do
  cborBa <- hexToByteArray cborHex
  hush $ toByteArray $ wrap $ wrap cborBa

decodeTextEnvelope
  :: String -> Maybe TextEnvelope
decodeTextEnvelope json = do
  aeson <- hush $ parseJsonStringToAeson json
  { "type": type_, description, cborHex } <-
    hush $ decodeAeson aeson :: _ TextEnvelopeRaw
  ba <- decodeCborHexToBytes cborHex
  pure $ wrap { type_, description, bytes: ba }

plutusScriptFromEnvelope :: TextEnvelope -> Maybe PlutusScript
plutusScriptFromEnvelope (TextEnvelope envelope) =
  case envelope.type_ of
    PlutusScriptV1 ->
      Just $ PlutusScript.plutusV1Script envelopeBytes
    PlutusScriptV2 ->
      Just $ PlutusScript.plutusV2Script envelopeBytes
    PlutusScriptV3 ->
      -- TODO: add plutusV3Script to Cardano.Types.PlutusScript
      Just $ wrap $ unwrap envelopeBytes /\ PlutusV3
    _ -> Nothing
  where
  envelopeBytes = wrap envelope.bytes
