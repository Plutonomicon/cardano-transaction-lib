module Contract.Crypto.Schnorr
  ( module X
  , verifySchnorrSecp256k1Signature
  , signSchnorrSecp256k1
  , deriveSchnorrSecp256k1PublicKey
  ) where

import Ctl.Internal.Types.ByteArray (ByteArray)
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
import Unsafe.Coerce (unsafeCoerce)

-- | Verify arbitrary binary messages signed using the Schnorr signature scheme on the SECP256k1 curve.
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
