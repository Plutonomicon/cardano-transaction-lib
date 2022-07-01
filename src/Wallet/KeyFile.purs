-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , parseJsonStringToAeson
  , (.:)
  )
import Control.Monad.Except (throwError)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.String.CodeUnits as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Helpers (liftM)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Serialization (privateKeyFromBytes)
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )

-- | Byte representation of CSL PrivateKey, can be decoded from JSON in
-- | `cardano-cli` format.
-- | (`PaymentSigningKeyShelley_ed25519`).
newtype PrivatePaymentKeyFile = PrivatePaymentKeyFile RawBytes

instance DecodeAeson PrivatePaymentKeyFile where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    typeStr <- obj .: "type"
    unless (typeStr == "PaymentSigningKeyShelley_ed25519") do
      throwError (TypeMismatch "PaymentSigningKeyShelley_ed25519")
    cborHex <- obj .: "cborHex"
    let splitted = String.splitAt 4 cborHex
    unless (splitted.before == "5820") do
      throwError (TypeMismatch "PrivateKeyFile CborHex")
    case hexToByteArray splitted.after of
      Nothing -> throwError (TypeMismatch "PrivateKey CborHex")
      Just byteArray -> pure $ PrivatePaymentKeyFile $ wrap $ byteArray

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  case (decodeAeson <=< parseJsonStringToAeson) fileContents of
    Left err -> liftEffect $ throw $ show err
    Right (PrivatePaymentKeyFile bytes) -> do
      liftM (error "Unable to decode private payment key") $
        PrivatePaymentKey <$> privateKeyFromBytes bytes

-- | Byte representation of CSL PrivateKey, can be decoded from JSON in
-- | `cardano-cli` format.
-- | (`StakeSigningKeyShelley_ed25519`).
newtype PrivateStakeKeyFile = PrivateStakeKeyFile RawBytes

instance DecodeAeson PrivateStakeKeyFile where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    typeStr <- obj .: "type"
    unless (typeStr == "StakeSigningKeyShelley_ed25519") do
      throwError (TypeMismatch "StakeSigningKeyShelley_ed25519")
    cborHex <- obj .: "cborHex"
    let splitted = String.splitAt 4 cborHex
    unless (splitted.before == "5820") do
      throwError (TypeMismatch "PrivateKeyFile CborHex")
    case hexToByteArray splitted.after of
      Nothing -> throwError (TypeMismatch "PrivateKey CborHex")
      Just byteArray -> pure $ PrivateStakeKeyFile $ wrap $ byteArray

privateStakeKeyFromFile :: FilePath -> Aff PrivateStakeKey
privateStakeKeyFromFile filePath = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  case (decodeAeson <=< parseJsonStringToAeson) fileContents of
    Left err -> liftEffect $ throw $ show err
    Right (PrivateStakeKeyFile bytes) -> do
      liftM (error "Unable to decode private stake key") $
        PrivateStakeKey <$> privateKeyFromBytes bytes
