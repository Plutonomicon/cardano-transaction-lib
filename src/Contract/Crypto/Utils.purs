module Contract.Crypto.Utils
  ( hashMessageSha256
  , derivePrivateKey
  , mkPrivateKey
  , unPrivateKey
  , module X
  ) where

import Prelude

import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (MessageHash, PrivateKey)
import Noble.Secp256k1.ECDSA as ECDSA
import Noble.Secp256k1.Utils (hashToPrivateKey, sha256)
import Noble.Secp256k1.Utils (randomPrivateKey) as X
import Unsafe.Coerce (unsafeCoerce)

-- | Hash a byte array using sha256, for use with `signEcdsaSecp256k1`
-- | and `verifyEcdsaSecp256k1Signature`.
hashMessageSha256 :: ByteArray -> Aff MessageHash
hashMessageSha256 = unwrap >>> unsafeCoerce >>> sha256

-- | Deterministically derive a private key given an array of bytes.
-- | Array size must be between 40 and 1024 bytes.
-- |
-- | Normally, the input bytes should be kept secret and shouldn't be
-- | easily reconstructible.
derivePrivateKey :: ByteArray -> Maybe PrivateKey
derivePrivateKey = unsafeCoerce >>> hashToPrivateKey

-- | Attempt to convert a byte array containing a byte-representation of a
-- | private key to a private key. Invalid values will be rejected.
mkPrivateKey :: ByteArray -> Maybe PrivateKey
mkPrivateKey = unwrap >>> ECDSA.mkPrivateKey

unPrivateKey :: PrivateKey -> ByteArray
unPrivateKey = wrap <<< ECDSA.unPrivateKey
