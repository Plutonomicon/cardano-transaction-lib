-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  ) where

import Prelude

import Aeson (parseJsonStringToAeson)
import Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeDecodeError(JsonDecodeError)
  , TextEnvelopeType
      ( PaymentSigningKeyShelley_ed25519
      , StakeSigningKeyShelley_ed25519
      )
  , decodeTextEnvelope
  , printTextEnvelopeDecodeError
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Helpers (liftM)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Serialization (privateKeyFromBytes)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  let
    eTextEnvelope = do
      aeson <- lmap JsonDecodeError $ parseJsonStringToAeson fileContents
      decodeTextEnvelope aeson
  case eTextEnvelope of
    Left err -> liftEffect $ throw $ printTextEnvelopeDecodeError err
    Right (TextEnvelope { type_, bytes }) -> do
      unless (type_ == PaymentSigningKeyShelley_ed25519) $ liftEffect $ throw $
        "Expected PaymentSigningKeyShelley_ed25519"
      liftM (error "Unable to decode private payment key") $
        PrivatePaymentKey <$> privateKeyFromBytes (wrap bytes)

privateStakeKeyFromFile :: FilePath -> Aff PrivateStakeKey
privateStakeKeyFromFile filePath = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  let
    eTextEnvelope = do
      aeson <- lmap JsonDecodeError $ parseJsonStringToAeson fileContents
      decodeTextEnvelope aeson
  case eTextEnvelope of
    Left err -> liftEffect $ throw $ printTextEnvelopeDecodeError err
    Right (TextEnvelope { type_, bytes }) -> do
      unless (type_ == StakeSigningKeyShelley_ed25519) $ liftEffect $ throw $
        "Expected StakeSigningKeyShelley_ed25519"
      liftM (error "Unable to decode private stake key") $
        PrivateStakeKey <$> privateKeyFromBytes (wrap bytes)
