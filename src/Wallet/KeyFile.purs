-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privateKeyFromFile
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
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Serialization.Types (PrivateKey)
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes)
import Serialization (privateKeyFromBytes)

-- | Byte representation of CSL PrivateKey, can be decoded from JSON.
-- | (`PaymentSigningKeyShelley_ed25519`).
newtype PrivateKeyFile = PrivateKeyFile RawBytes

instance DecodeAeson PrivateKeyFile where
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
      Just byteArray -> pure $ PrivateKeyFile $ wrap $ byteArray

privateKeyFromFile :: FilePath -> Aff (Maybe PrivateKey)
privateKeyFromFile filePath = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  case (decodeAeson <=< parseJsonStringToAeson) fileContents of
    Left err -> liftEffect $ throw $ show err
    Right (PrivateKeyFile bytes) -> do
      pure $ privateKeyFromBytes bytes
