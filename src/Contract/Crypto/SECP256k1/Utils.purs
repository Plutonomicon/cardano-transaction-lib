module Contract.Crypto.SECP256k1.Utils
  ( hashMessageSha256
  , derivePrivateKey
  , mkPrivateKey
  , unPrivateKey
  , randomPrivateKey
  ) where

import Prelude

import Contract.Crypto.SECP256k1 (SECP256k1PrivateKey)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (MessageHash)
import Noble.Secp256k1.ECDSA as ECDSA
import Noble.Secp256k1.Utils (hashToPrivateKey, sha256)
import Noble.Secp256k1.Utils as Utils

randomPrivateKey :: Effect SECP256k1PrivateKey
randomPrivateKey = wrap <$> Utils.randomPrivateKey

-- | Hash a byte array using sha256, for use with `signEcdsaSecp256k1`
-- | and `verifyEcdsaSecp256k1Signature`.
hashMessageSha256 :: ByteArray -> Aff MessageHash
hashMessageSha256 = unwrap >>> sha256

-- | Deterministically derive a private key given an array of bytes.
-- | Array size must be between 40 and 1024 bytes.
-- |
-- | Normally, the input bytes should be kept secret and shouldn't be
-- | easily reconstructible.
derivePrivateKey :: ByteArray -> Maybe SECP256k1PrivateKey
derivePrivateKey = unwrap >>> hashToPrivateKey >>> map wrap

-- | Attempt to convert a byte array containing a byte-representation of a
-- | private key to a private key. Invalid values will be rejected.
mkPrivateKey :: ByteArray -> Maybe SECP256k1PrivateKey
mkPrivateKey = unwrap >>> ECDSA.mkPrivateKey >>> map wrap

unPrivateKey :: SECP256k1PrivateKey -> ByteArray
unPrivateKey = wrap <<< ECDSA.unPrivateKey <<< unwrap
