-- | **NodeJS-only module**
module Ctl.Internal.Wallet.KeyFile
  ( keyFromFile
  , privatePaymentKeyFromFile
  , privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyFromTextEnvelope
  , privateStakeKeyToFile
  , formatStakeKey
  , formatPaymentKey
  ) where

import Prelude

import Aeson (encodeAeson)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (catchError)
import Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , decodeTextEnvelope
  )
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Serialization.Keys (bytesFromPrivateKey)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath)

keyFromFile :: FilePath -> TextEnvelopeType -> Aff ByteArray
keyFromFile filePath ty = errorHandler do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  let errorMsg = error "Error while decoding key"
  liftMaybe errorMsg do
    TextEnvelope envelope <- decodeTextEnvelope fileContents
    -- Check TextEnvelope type match to desirable
    unless (envelope.type_ == ty) Nothing
    pure envelope.bytes
  where
  errorHandler action = do
    catchError action
      ( \err -> do
          liftEffect $ throw $
            "Unable to load key from file: " <> show filePath
              <> ", error: "
              <> show err
      )

privatePaymentKeyFromTextEnvelope :: TextEnvelope -> Maybe PrivatePaymentKey
privatePaymentKeyFromTextEnvelope (TextEnvelope envelope) = do
  -- Check TextEnvelope type match to desirable
  unless (envelope.type_ == PaymentSigningKeyShelleyed25519) Nothing
  PrivatePaymentKey <$> privateKeyFromBytes (wrap envelope.bytes)

privateStakeKeyFromTextEnvelope :: TextEnvelope -> Maybe PrivateStakeKey
privateStakeKeyFromTextEnvelope (TextEnvelope envelope) = do
  -- Check TextEnvelope type match to desirable
  unless (envelope.type_ == StakeSigningKeyShelleyed25519) Nothing
  PrivateStakeKey <$> privateKeyFromBytes (wrap envelope.bytes)

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  bytes <- keyFromFile filePath PaymentSigningKeyShelleyed25519
  liftM (error "Unable to decode private payment key") $
    PrivatePaymentKey <$> privateKeyFromBytes (wrap bytes)

privateStakeKeyFromFile :: FilePath -> Aff PrivateStakeKey
privateStakeKeyFromFile filePath = do
  bytes <- keyFromFile filePath StakeSigningKeyShelleyed25519
  liftM (error "Unable to decode private stake key") $
    PrivateStakeKey <$> privateKeyFromBytes (wrap bytes)

-- | Write private payment key to file in cardano-cli envelope format
privatePaymentKeyToFile :: FilePath -> PrivatePaymentKey -> Aff Unit
privatePaymentKeyToFile filePath key =
  liftEffect <<< (writeTextFile Encoding.UTF8 filePath) $ formatPaymentKey key

-- | Write private stake key to file in cardano-cli envelope format
privateStakeKeyToFile :: FilePath -> PrivateStakeKey -> Aff Unit
privateStakeKeyToFile filePath key =
  liftEffect <<< (writeTextFile Encoding.UTF8 filePath) $ formatStakeKey key

-- | Convert private payment key to cardano-cli envelope format.
formatPaymentKey :: PrivatePaymentKey -> String
formatPaymentKey (PrivatePaymentKey key) = encodeAeson >>> show
  $
    { "type": "PaymentSigningKeyShelley_ed25519"
    , description: "Payment Signing Key"
    , cborHex: keyToCbor key
    }

-- | Convert private stake key to cardano-cli envelope format.
formatStakeKey :: PrivateStakeKey -> String
formatStakeKey (PrivateStakeKey key) = encodeAeson >>> show
  $
    { "type": "StakeSigningKeyShelley_ed25519"
    , description: "Stake Signing Key"
    , cborHex: keyToCbor key
    }

keyToCbor :: PrivateKey -> String
keyToCbor = (magicPrefix <> _) <<< rawBytesToHex <<< bytesFromPrivateKey

magicPrefix :: String
magicPrefix = "5820"
