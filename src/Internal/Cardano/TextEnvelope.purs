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
  , plutusScriptV1FromEnvelope
  , plutusScriptV2FromEnvelope
  , plutusScriptV3FromEnvelope
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , decodeAeson
  , parseJsonStringToAeson
  )
import Ctl.Internal.Types.ByteArray (ByteArray, hexToByteArray)
import Ctl.Internal.Types.Cbor (toByteArray)
import Ctl.Internal.Types.Scripts
  ( PlutusScript
  , plutusV1Script
  , plutusV2Script
  , plutusV3Script
  )
import Data.Either (hush)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)

data TextEnvelopeType
  = PlutusScriptV1
  | PlutusScriptV2
  | PlutusScriptV3
  | PaymentSigningKeyShelleyed25519
  | StakeSigningKeyShelleyed25519
  | Other String -- TextEnvelope we can parse from String, but cannot use now

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

plutusScriptFromEnvelope
  :: TextEnvelopeType
  -> (ByteArray -> PlutusScript)
  -> TextEnvelope
  -> Maybe PlutusScript
plutusScriptFromEnvelope type_ bytesToScript (TextEnvelope envelope) = do
  -- Check TextEnvelope type match to desirable
  unless (envelope.type_ == type_) Nothing
  pure $ bytesToScript envelope.bytes

plutusScriptV1FromEnvelope
  :: TextEnvelope -> Maybe PlutusScript
plutusScriptV1FromEnvelope envelope = do
  plutusScriptFromEnvelope PlutusScriptV1 plutusV1Script envelope

plutusScriptV2FromEnvelope
  :: TextEnvelope -> Maybe PlutusScript
plutusScriptV2FromEnvelope envelope =
  plutusScriptFromEnvelope PlutusScriptV2 plutusV2Script envelope

plutusScriptV3FromEnvelope
  :: TextEnvelope -> Maybe PlutusScript
plutusScriptV3FromEnvelope envelope =
  plutusScriptFromEnvelope PlutusScriptV3 plutusV3Script envelope
