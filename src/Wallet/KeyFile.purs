-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  , keyFromFile
  , privatePaymentKeyToFile
  , privateStakeKeyToFile
  ) where

import Prelude

import Aeson (encodeAeson)
import Cardano.TextEnvelope
  ( TextEnvelopeType
      ( PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , textEnvelopeBytes
  )
import Data.Newtype (wrap)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Helpers (liftM)
import Node.Encoding as Encoding
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Serialization (privateKeyFromBytes, bytesFromPrivateKey)
import Serialization.Types (PrivateKey)
import Types.ByteArray (ByteArray)
import Types.RawBytes (rawBytesToHex)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )

keyFromFile :: FilePath -> TextEnvelopeType -> Aff ByteArray
keyFromFile filePath ty = do
  fileContents <- liftEffect $ readTextFile Encoding.UTF8 filePath
  textEnvelopeBytes fileContents ty

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

privatePaymentKeyToFile :: FilePath -> PrivatePaymentKey -> Aff Unit
privatePaymentKeyToFile filePath key =
  liftM (error "failed to format key") (formatPaymentKey key) >>=
    liftEffect <<< (writeTextFile Encoding.UTF8 filePath)

privateStakeKeyToFile :: FilePath -> PrivateStakeKey -> Aff Unit
privateStakeKeyToFile filePath key =
  liftM (error "failed to format key") (formatStakeKey key) >>=
    liftEffect <<< (writeTextFile Encoding.UTF8 filePath)

formatPaymentKey :: PrivatePaymentKey -> Maybe String
formatPaymentKey (PrivatePaymentKey key) = encodeAeson >>> show
  <$>
    { "type": "PaymentSigningKeyShelley_ed25519"
    , description: "Payment Signing Key"
    , cborHex: _
    }
  <$> keyToCbor key

formatStakeKey :: PrivateStakeKey -> Maybe String
formatStakeKey (PrivateStakeKey key) = encodeAeson >>> show
  <$>
    { "type": "StakeSigningKeyShelley_ed25519"
    , description: "Stake Signing Key"
    , cborHex: _
    }
  <$> keyToCbor key

keyToCbor :: PrivateKey -> Maybe String
keyToCbor = ((rawBytesToHex >>> (magicPrefix <> _)) <$> _) <<<
  bytesFromPrivateKey

magicPrefix :: String
magicPrefix = "5820"
