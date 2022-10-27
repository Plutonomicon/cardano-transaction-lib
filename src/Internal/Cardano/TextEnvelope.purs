module Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  , plutusScriptV2FromEnvelope
  , textEnvelopeBytes
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , parseJsonStringToAeson
  , printJsonDecodeError
  )
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (throwError)
import Ctl.Internal.Deserialization.FromBytes (FromBytesError, fromBytes')
import Ctl.Internal.Error (E)
import Ctl.Internal.Serialization.Types as ST
import Ctl.Internal.Types.ByteArray (ByteArray, hexToByteArray)
import Ctl.Internal.Types.Cbor (CborParseError, toByteArray)
import Ctl.Internal.Types.Scripts
  ( PlutusScript
  , plutusV1Script
  , plutusV2Script
  )
import Data.Bifunctor (bimap, lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Variant (match)
import Type.Row (type (+))

data TextEnvelopeType
  = PlutusScriptV1
  | PlutusScriptV2
  | PaymentSigningKeyShelleyed25519
  | StakeSigningKeyShelleyed25519

derive instance Eq TextEnvelopeType

instance Show TextEnvelopeType where
  show = case _ of
    PlutusScriptV1 -> "PlutusScriptV1"
    PlutusScriptV2 -> "PlutusScriptV2"
    PaymentSigningKeyShelleyed25519 -> "PaymentSigningKeyShelley_ed25519"
    StakeSigningKeyShelleyed25519 -> "StakeSigningKeyShelley_ed25519"

instance DecodeAeson TextEnvelopeType where
  decodeAeson aeson = do
    decodeAeson aeson >>= case _ of
      "PlutusScriptV1" -> pure PlutusScriptV1
      "PlutusScriptV2" -> pure PlutusScriptV2
      "PaymentSigningKeyShelley_ed25519" -> pure
        PaymentSigningKeyShelleyed25519
      "StakeSigningKeyShelley_ed25519" -> pure
        StakeSigningKeyShelleyed25519
      _ -> throwError $ TypeMismatch "TextEnvelopeType"

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
  :: Aeson -> Maybe TextEnvelope
decodeTextEnvelope aeson = do
  { "type": type_, description, cborHex } <-
    hush $ decodeAeson aeson :: _ TextEnvelopeRaw
  ba <- decodeCborHexToBytes cborHex
  pure $ wrap { type_, description, bytes: ba }

textEnvelopeBytes
  :: String -> TextEnvelopeType -> Maybe ByteArray
textEnvelopeBytes json ty = do
  aeson <- hush $ parseJsonStringToAeson json
  TextEnvelope te <- decodeTextEnvelope aeson
  if (te.type_ == ty) then pure te.bytes else Nothing

_plutusScriptVxFromEnvelope
  :: TextEnvelopeType
  -> (ByteArray -> PlutusScript)
  -> String
  -> Maybe PlutusScript
_plutusScriptVxFromEnvelope version bytesToScript str = do
  bytes <- textEnvelopeBytes str version
  map
    (bytesToScript <<< ST.bytes)
    (hush ((fromBytes' bytes) :: E (FromBytesError + ()) ST.PlutusScript))

plutusScriptV1FromEnvelope
  :: String -> Maybe PlutusScript
plutusScriptV1FromEnvelope str = do
  _plutusScriptVxFromEnvelope PlutusScriptV1 plutusV1Script str

plutusScriptV2FromEnvelope
  :: String -> Maybe PlutusScript
plutusScriptV2FromEnvelope str =
  _plutusScriptVxFromEnvelope PlutusScriptV2 plutusV2Script str
