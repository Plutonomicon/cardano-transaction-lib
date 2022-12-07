module Contract.Crypto.Utils
  ( hashMessageSha256
  , messageHashToByteArray
  , derivePrivateKey
  , module X
  ) where

import Prelude

import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Noble.Secp256k1.ECDSA (MessageHash, PrivateKey)
import Noble.Secp256k1.Utils (hashToPrivateKey, sha256)
import Noble.Secp256k1.Utils (randomPrivateKey) as X
import Unsafe.Coerce (unsafeCoerce)

-- | Hash a byte array using sha256, for use with `signEcdsaSecp256k1`
-- | and `verifyEcdsaSecp256k1Signature`.
hashMessageSha256 :: ByteArray -> Aff MessageHash
hashMessageSha256 = unwrap >>> unsafeCoerce >>> sha256

messageHashToByteArray :: MessageHash -> ByteArray
messageHashToByteArray =
  -- we know that the representation is the same: Uint8Array
  unsafeCoerce

-- | Deterministically derive a private key given an array of bytes.
-- | Array size must be between 40 and 1024 bytes.
-- |
-- | Normally, the input bytes should be kept secret and shouldn't be
-- | easily reconstructible.
derivePrivateKey :: ByteArray -> Maybe PrivateKey
derivePrivateKey = unsafeCoerce >>> hashToPrivateKey
