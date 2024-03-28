-- | A module that implements crypto primitives that match CIP-49 SECP256k1
-- | ECDSA spec.
module Contract.Crypto.Secp256k1.ECDSA
  ( verifyEcdsaSecp256k1Signature
  , signEcdsaSecp256k1
  , deriveEcdsaSecp256k1PublicKey
  , module X
  , mkECDSAPublicKey
  , unECDSAPublicKey
  , mkMessageHash
  , unMessageHash
  ) where

import Prelude

import Contract.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (ECDSAPublicKey, ECDSASignature, MessageHash) as X
import Noble.Secp256k1.ECDSA
  ( ECDSAPublicKey
  , ECDSASignature
  , MessageHash
  , signECDSA
  , verifyECDSA
  )
import Noble.Secp256k1.ECDSA
  ( getECDSAPublicKey
  , mkECDSAPublicKey
  , mkMessageHash
  , unECDSAPublicKey
  , unMessageHash
  ) as ECDSA

-- | Verify ECDSA signature for a message hash.
-- |
-- | This function matches CIP-49 definition:
-- |
-- | https://github.com/cardano-foundation/CIPs/blob/master/CIP-0049/README.md
verifyEcdsaSecp256k1Signature
  :: ECDSAPublicKey -> MessageHash -> ECDSASignature -> Boolean
verifyEcdsaSecp256k1Signature publicKey messageHash signature =
  verifyECDSA signature messageHash publicKey

-- | Sign a message hash with a private key, producing a signature compatible
-- | with `verifyEcdsaSecp256k1Signature`.
signEcdsaSecp256k1 :: Secp256k1PrivateKey -> MessageHash -> Aff ECDSASignature
signEcdsaSecp256k1 privateKey messageHash =
  signECDSA messageHash (unwrap privateKey) false

-- | Derive a public key from a private key. Uses `SECP256K1_EC_COMPRESSED`
-- | format (compatible with CIP-49).
deriveEcdsaSecp256k1PublicKey :: Secp256k1PrivateKey -> ECDSAPublicKey
deriveEcdsaSecp256k1PublicKey = unwrap >>> flip ECDSA.getECDSAPublicKey true

-- | Construct a public key from its byte representation.
mkECDSAPublicKey :: ByteArray -> Maybe ECDSAPublicKey
mkECDSAPublicKey = ECDSA.mkECDSAPublicKey

unECDSAPublicKey :: ECDSAPublicKey -> ByteArray
unECDSAPublicKey = ECDSA.unECDSAPublicKey

-- | Construct a message hash from its byte representation.
-- |
-- | This function **DOES NOT** compute the hash.
-- | Use `Contract.Crypto.Utils.hashMessageSha256` for that.
mkMessageHash :: ByteArray -> Maybe MessageHash
mkMessageHash = ECDSA.mkMessageHash

unMessageHash :: MessageHash -> ByteArray
unMessageHash = ECDSA.unMessageHash
