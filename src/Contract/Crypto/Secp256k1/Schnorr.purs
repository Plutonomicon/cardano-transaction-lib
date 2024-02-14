-- | A module that implements crypto primitives that match CIP-49 SECP256k1
-- | Schnorr spec.
module Contract.Crypto.Secp256k1.Schnorr
  ( module X
  , verifySchnorrSecp256k1Signature
  , signSchnorrSecp256k1
  , deriveSchnorrSecp256k1PublicKey
  , mkSchnorrPublicKey
  , unSchnorrPublicKey
  ) where

import Prelude

import Contract.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Noble.Secp256k1.Schnorr (SchnorrPublicKey, SchnorrSignature) as X
import Noble.Secp256k1.Schnorr
  ( SchnorrPublicKey
  , SchnorrSignature
  , getSchnorrPublicKey
  , signSchnorr
  , verifySchnorr
  )
import Noble.Secp256k1.Schnorr (mkSchnorrPublicKey, unSchnorrPublicKey) as ECDSA

-- | Verify arbitrary binary messages signed using the Schnorr signature scheme
-- | on the Secp256k1 curve.
-- | Matches CIP-49 spec:
-- | https://github.com/cardano-foundation/CIPs/blob/master/CIP-0049/README.md
verifySchnorrSecp256k1Signature
  :: SchnorrPublicKey -> ByteArray -> SchnorrSignature -> Aff Boolean
verifySchnorrSecp256k1Signature publicKey message signature =
  verifySchnorr signature (unwrap message) publicKey

-- | Sign a message using Schnorr signature scheme.
signSchnorrSecp256k1 :: Secp256k1PrivateKey -> ByteArray -> Aff SchnorrSignature
signSchnorrSecp256k1 privateKey message =
  signSchnorr (unwrap message) (unwrap privateKey)

deriveSchnorrSecp256k1PublicKey :: Secp256k1PrivateKey -> SchnorrPublicKey
deriveSchnorrSecp256k1PublicKey = unwrap >>> getSchnorrPublicKey

-- | Construct a public key from its byte representation.
mkSchnorrPublicKey
  :: ByteArray -> Maybe SchnorrPublicKey
mkSchnorrPublicKey = unwrap >>> ECDSA.mkSchnorrPublicKey

unSchnorrPublicKey :: SchnorrPublicKey -> ByteArray
unSchnorrPublicKey = wrap <<< ECDSA.unSchnorrPublicKey
