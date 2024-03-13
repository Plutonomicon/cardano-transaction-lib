-- | Exposes constructors for `PublicKey` and `Ed25519Signature` types
module Contract.Keys
  ( privateKeyFromBech32
  , privateKeyFromBytes
  , publicKeyFromBech32
  , publicKeyFromBytes
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Serialization.Lib
  ( privateKey_fromBech32
  , privateKey_fromNormalBytes
  , publicKey_fromBytes
  ) as Csl
import Cardano.Types (Bech32String, RawBytes(..))
import Cardano.Types.PrivateKey (PrivateKey(..))
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey (PublicKey(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Safe.Coerce (coerce)

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = PrivateKey.fromRawBytes

privateKeyFromBech32 :: Bech32String -> Maybe PrivateKey
privateKeyFromBech32 = PrivateKey.fromBech32

publicKeyFromBytes :: RawBytes -> Maybe PublicKey
publicKeyFromBytes =
  decodeCbor <<< wrap <<< unwrap

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = PublicKey.fromBech32
