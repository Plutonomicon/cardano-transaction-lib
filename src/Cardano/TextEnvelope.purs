module Cardano.TextEnvelope
  ( decodeTextEnvelope
  , printTextEnvelopeDecodeError
  , textEnvelopeBytes
  , TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PlutusScriptV2
      , PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
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
import Control.Monad.Except (throwError)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Newtype (class Newtype, wrap)
import Effect.Aff (Aff, error)
import Types.ByteArray (ByteArray, hexToByteArray)
import Types.Cbor (CborParseError, toByteArray)

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

data TextEnvelopeDecodeError
  = JsonDecodeError JsonDecodeError
  | CborParseError CborParseError

decodeTextEnvelope
  :: Aeson -> Either TextEnvelopeDecodeError TextEnvelope
decodeTextEnvelope aeson = do
  { "type": type_, description, cborHex } <-
    lmap JsonDecodeError $ decodeAeson aeson :: _ TextEnvelopeRaw
  cborBa <- note (JsonDecodeError $ TypeMismatch "Hex") $ hexToByteArray cborHex
  -- NOTE PlutusScriptV1 is doubly-encoded cbor, so the resulting `ByteArray`
  -- is *still* cbor encoded.
  -- https://github.com/Emurgo/cardano-serialization-lib/issues/268#issuecomment-1042986055
  ba <- lmap CborParseError $ toByteArray $ wrap $ wrap cborBa
  pure $ wrap { type_, description, bytes: ba }

printTextEnvelopeDecodeError :: TextEnvelopeDecodeError -> String
printTextEnvelopeDecodeError = case _ of
  JsonDecodeError err -> printJsonDecodeError err
  CborParseError err -> show err

textEnvelopeBytes :: String -> TextEnvelopeType -> Aff ByteArray
textEnvelopeBytes json ty =
  liftEither $ lmap (error <<< printTextEnvelopeDecodeError) do
    aeson <- lmap JsonDecodeError $ parseJsonStringToAeson json
    TextEnvelope te <- decodeTextEnvelope aeson
    unless (te.type_ == ty) $ throwError $ JsonDecodeError $ TypeMismatch $
      show ty
    pure te.bytes
