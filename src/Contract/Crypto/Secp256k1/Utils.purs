module Contract.Crypto.Secp256k1.Utils
  ( hashMessageSha256
  , deriveSecp256k1PrivateKey
  , mkSecp256k1PrivateKey
  , unSecp256k1PrivateKey
  , randomSecp256k1PrivateKey
  ) where

import Prelude

import Contract.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Data.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (MessageHash)
import Noble.Secp256k1.ECDSA as ECDSA
import Noble.Secp256k1.Utils (hashToPrivateKey, sha256)
import Noble.Secp256k1.Utils as Utils

randomSecp256k1PrivateKey :: Effect Secp256k1PrivateKey
randomSecp256k1PrivateKey = wrap <$> Utils.randomPrivateKey

-- | Hash a byte array using sha256, for use with `signEcdsaSecp256k1`
-- | and `verifyEcdsaSecp256k1Signature`.
hashMessageSha256 :: ByteArray -> Aff MessageHash
hashMessageSha256 = unwrap >>> sha256

-- | Deterministically derive a private key given an array of bytes.
-- | Array size must be between 40 and 1024 bytes.
-- |
-- | Normally, the input bytes should be kept secret and shouldn't be
-- | easily reconstructible.
deriveSecp256k1PrivateKey :: ByteArray -> Maybe Secp256k1PrivateKey
deriveSecp256k1PrivateKey = unwrap >>> hashToPrivateKey >>> map wrap

-- | Attempt to convert a byte array containing a byte-representation of a
-- | private key to a private key. Invalid values will be rejected.
mkSecp256k1PrivateKey :: ByteArray -> Maybe Secp256k1PrivateKey
mkSecp256k1PrivateKey = unwrap >>> ECDSA.mkPrivateKey >>> map wrap

unSecp256k1PrivateKey :: Secp256k1PrivateKey -> ByteArray
unSecp256k1PrivateKey = wrap <<< ECDSA.unPrivateKey <<< unwrap
