module Cardano.TextEnvelope
  ( decodeTextEnvelope
  , printTextEnvelopeDecodeError
  , TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PlutusScriptV1
      , PaymentSigningKeyShelley_ed25519
      , StakeSigningKeyShelley_ed25519
      )
  , TextEnvelopeDecodeError(JsonDecodeError, CborParseError)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  )
import Control.Monad.Except (throwError)
import Data.Argonaut (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Newtype (class Newtype, wrap)
import Types.ByteArray (ByteArray, hexToByteArray)
import Types.Cbor (CborParseError, toByteArray)

data TextEnvelopeType
  = PlutusScriptV1
  | PaymentSigningKeyShelley_ed25519
  | StakeSigningKeyShelley_ed25519

derive instance Eq TextEnvelopeType

instance Show TextEnvelopeType where
  show = case _ of
    PlutusScriptV1 -> "PlutusScriptV1"
    PaymentSigningKeyShelley_ed25519 -> "PaymentSigningKeyShelley_ed25519"
    StakeSigningKeyShelley_ed25519 -> "StakeSigningKeyShelley_ed25519"

instance DecodeAeson TextEnvelopeType where
  decodeAeson aeson = do
    decodeAeson aeson >>= case _ of
      "PlutusScriptV1" -> pure PlutusScriptV1
      "PaymentSigningKeyShelley_ed25519" -> pure
        PaymentSigningKeyShelley_ed25519
      "StakeSigningKeyShelley_ed25519" -> pure
        StakeSigningKeyShelley_ed25519
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
  ba <- lmap CborParseError $ toByteArray $ wrap $ wrap cborBa
  pure $ wrap { type_, description, bytes: ba }

printTextEnvelopeDecodeError :: TextEnvelopeDecodeError -> String
printTextEnvelopeDecodeError = case _ of
  JsonDecodeError err -> printJsonDecodeError err
  CborParseError err -> show err

