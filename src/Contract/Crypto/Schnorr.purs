-- | A module that implements crypto primitives that match CIP-49 SECP256k1
-- | Schnorr spec.
module Contract.Crypto.Schnorr
  ( module X
  , verifySchnorrSecp256k1Signature
  , signSchnorrSecp256k1
  , deriveSchnorrSecp256k1PublicKey
  , mkSchnorrPublicKey
  , unSchnorrPublicKey
  ) where

import Prelude

import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (PrivateKey)
import Noble.Secp256k1.Schnorr (PrivateKey, SchnorrPublicKey, SchnorrSignature) as X
import Noble.Secp256k1.Schnorr
  ( SchnorrPublicKey
  , SchnorrSignature
  , getSchnorrPublicKey
  , signSchnorr
  , verifySchnorr
  )
import Noble.Secp256k1.Schnorr (mkSchnorrPublicKey, unSchnorrPublicKey) as ECDSA
import Unsafe.Coerce (unsafeCoerce)

-- | Verify arbitrary binary messages signed using the Schnorr signature scheme
-- | on the SECP256k1 curve.
-- | Matches CIP-49 spec:
-- | https://github.com/cardano-foundation/CIPs/blob/master/CIP-0049/README.md
verifySchnorrSecp256k1Signature
  :: SchnorrPublicKey -> ByteArray -> SchnorrSignature -> Aff Boolean
verifySchnorrSecp256k1Signature publicKey message signature =
  verifySchnorr signature (unsafeCoerce message) publicKey

-- | Sign a message using Schnorr signature scheme.
signSchnorrSecp256k1
  :: PrivateKey -> ByteArray -> Aff SchnorrSignature
signSchnorrSecp256k1 privateKey message = signSchnorr (unsafeCoerce message)
  privateKey

deriveSchnorrSecp256k1PublicKey :: PrivateKey -> SchnorrPublicKey
deriveSchnorrSecp256k1PublicKey = getSchnorrPublicKey

-- | Construct a public key from its byte representation.
mkSchnorrPublicKey
  :: ByteArray -> Maybe SchnorrPublicKey
mkSchnorrPublicKey = unwrap >>> ECDSA.mkSchnorrPublicKey

unSchnorrPublicKey :: SchnorrPublicKey -> ByteArray
unSchnorrPublicKey = wrap <<< ECDSA.unSchnorrPublicKey
