-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privateKeyFromFile
  ) where

import Prelude

import Aeson (parseJsonStringToAeson)
import Cardano.TextEnvelope
  ( TextEnvelope(..)
  , TextEnvelopeDecodeError(..)
  , TextEnvelopeType(..)
  , decodeTextEnvelope
  , printTextEnvelopeDecodeError
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Serialization (privateKeyFromBytes)
import Serialization.Types (PrivateKey)

privateKeyFromFile :: FilePath -> Aff (Maybe PrivateKey)
privateKeyFromFile filePath = do
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
      pure $ privateKeyFromBytes $ wrap bytes
