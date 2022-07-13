-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  , keyFromFile
  ) where

import Prelude

import Aeson (parseJsonStringToAeson, JsonDecodeError(TypeMismatch))
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
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Error.Class (liftEither)
import Helpers (liftM)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Serialization (privateKeyFromBytes)
import Types.ByteArray (ByteArray)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )

keyFromFile :: FilePath -> TextEnvelopeType -> Aff ByteArray
keyFromFile filePath ty = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  liftEither $ lmap (error <<< printTextEnvelopeDecodeError) do
    aeson <- lmap JsonDecodeError $ parseJsonStringToAeson fileContents
    TextEnvelope te <- decodeTextEnvelope aeson
    unless (te.type_ == ty) $ throwError $ JsonDecodeError $ TypeMismatch $
      show ty
    pure te.bytes

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  bytes <- keyFromFile filePath PaymentSigningKeyShelley_ed25519
  liftM (error "Unable to decode private payment key") $
    PrivatePaymentKey <$> privateKeyFromBytes (wrap bytes)

privateStakeKeyFromFile :: FilePath -> Aff PrivateStakeKey
privateStakeKeyFromFile filePath = do
  bytes <- keyFromFile filePath StakeSigningKeyShelley_ed25519
  liftM (error "Unable to decode private stake key") $
    PrivateStakeKey <$> privateKeyFromBytes (wrap bytes)
