-- | **NodeJS-only module**
module Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  , keyFromFile
  ) where

import Prelude

import Cardano.TextEnvelope
  ( TextEnvelopeType
      ( PaymentSigningKeyShelleyed25519
      , StakeSigningKeyShelleyed25519
      )
  , textEnvelopeBytes
  )
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
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
