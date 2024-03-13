-- | Exposes constructors for `PublicKey` and `Ed25519Signature` types
module Contract.Keys
  ( privateKeyFromBech32
  , privateKeyFromBytes
  , publicKeyFromBech32
  , publicKeyFromBytes
  ) where

import Cardano.Types (Bech32String, RawBytes)
import Cardano.Types.PrivateKey (PrivateKey)
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey (PublicKey)
import Cardano.Types.PublicKey as PublicKey
import Data.Maybe (Maybe)

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = PrivateKey.fromRawBytes

privateKeyFromBech32 :: Bech32String -> Maybe PrivateKey
privateKeyFromBech32 = PrivateKey.fromBech32

publicKeyFromBytes :: RawBytes -> Maybe PublicKey
publicKeyFromBytes =
  PublicKey.fromRawBytes

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = PublicKey.fromBech32
