-- | **NodeJS-only module**
module Ctl.Internal.Wallet.KeyFile
  ( keyFromFile
  , privateDrepKeyFromFile
  , privateDrepKeyFromTextEnvelope
  , privateDrepKeyToFile
  , privatePaymentKeyFromFile
  , privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyFromTextEnvelope
  , privateStakeKeyToFile
  , formatDrepKey
  , formatPaymentKey
  , formatStakeKey
  ) where

import Prelude

import Aeson (encodeAeson)
import Cardano.Types.PrivateKey (PrivateKey)
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Wallet.Key
  ( PrivateDrepKey(PrivateDrepKey)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (catchError)
import Ctl.Internal.Cardano.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType
      ( PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      , DRepSigningKeyed25519
      )
  , decodeTextEnvelope
  )
import Ctl.Internal.Helpers (liftM)
import Data.ByteArray (ByteArray, byteArrayToHex)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
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
  unless (envelope.type_ == PaymentSigningKeyShelleyed25519) Nothing
  PrivatePaymentKey <$> PrivateKey.fromRawBytes (wrap envelope.bytes)

privateStakeKeyFromTextEnvelope :: TextEnvelope -> Maybe PrivateStakeKey
privateStakeKeyFromTextEnvelope (TextEnvelope envelope) = do
  unless (envelope.type_ == StakeSigningKeyShelleyed25519) Nothing
  PrivateStakeKey <$> PrivateKey.fromRawBytes (wrap envelope.bytes)

privateDrepKeyFromTextEnvelope :: TextEnvelope -> Maybe PrivateDrepKey
privateDrepKeyFromTextEnvelope (TextEnvelope envelope) = do
  unless (envelope.type_ == DRepSigningKeyed25519) Nothing
  PrivateDrepKey <$> PrivateKey.fromRawBytes (wrap envelope.bytes)

privatePaymentKeyFromFile :: FilePath -> Aff PrivatePaymentKey
privatePaymentKeyFromFile filePath = do
  bytes <- keyFromFile filePath PaymentSigningKeyShelleyed25519
  liftM (error "Unable to decode private payment key") $
    PrivatePaymentKey <$> PrivateKey.fromRawBytes (wrap bytes)

privateStakeKeyFromFile :: FilePath -> Aff PrivateStakeKey
privateStakeKeyFromFile filePath = do
  bytes <- keyFromFile filePath StakeSigningKeyShelleyed25519
  liftM (error "Unable to decode private stake key") $
    PrivateStakeKey <$> PrivateKey.fromRawBytes (wrap bytes)

privateDrepKeyFromFile :: FilePath -> Aff PrivateDrepKey
privateDrepKeyFromFile filePath = do
  bytes <- keyFromFile filePath DRepSigningKeyed25519
  liftM (error "Unable to decode private DRep key") $
    PrivateDrepKey <$> PrivateKey.fromRawBytes (wrap bytes)

-- | Write private payment key to file in cardano-cli envelope format
privatePaymentKeyToFile :: FilePath -> PrivatePaymentKey -> Aff Unit
privatePaymentKeyToFile filePath =
  liftEffect
    <<< writeTextFile Encoding.UTF8 filePath
    <<< formatPaymentKey

-- | Write private stake key to file in cardano-cli envelope format
privateStakeKeyToFile :: FilePath -> PrivateStakeKey -> Aff Unit
privateStakeKeyToFile filePath =
  liftEffect
    <<< writeTextFile Encoding.UTF8 filePath
    <<< formatStakeKey

-- | Write private DRep key to file in cardano-cli envelope format
privateDrepKeyToFile :: FilePath -> PrivateDrepKey -> Aff Unit
privateDrepKeyToFile filePath =
  liftEffect
    <<< writeTextFile Encoding.UTF8 filePath
    <<< formatDrepKey

-- | Convert private payment key to cardano-cli envelope format.
formatPaymentKey :: PrivatePaymentKey -> String
formatPaymentKey (PrivatePaymentKey key) =
  show $ encodeAeson
    { "type": "PaymentSigningKeyShelley_ed25519"
    , description: "Payment Signing Key"
    , cborHex: keyToCbor key
    }

-- | Convert private stake key to cardano-cli envelope format.
formatStakeKey :: PrivateStakeKey -> String
formatStakeKey (PrivateStakeKey key) =
  show $ encodeAeson
    { "type": "StakeSigningKeyShelley_ed25519"
    , description: "Stake Signing Key"
    , cborHex: keyToCbor key
    }

-- | Convert private DRep key to cardano-cli envelope format.
formatDrepKey :: PrivateDrepKey -> String
formatDrepKey (PrivateDrepKey key) =
  show $ encodeAeson
    { "type": "DRepSigningKey_ed25519"
    , description: "Delegated Representative Signing Key"
    , cborHex: keyToCbor key
    }

keyToCbor :: PrivateKey -> String
keyToCbor =
  (magicPrefix <> _) <<< byteArrayToHex <<< unwrap <<< PrivateKey.toRawBytes

magicPrefix :: String
magicPrefix = "5820"
