-- | Exposes constructors for `PublicKey` and `Ed25519Signature` types
module Contract.Keys
  ( module X
  , privateKeyFromBech32
  , privateKeyFromBytes
  , publicKeyFromBech32
  , publicKeyFromBytes
  ) where

import Prelude

import Cardano.Serialization.Lib
  ( privateKey_fromBech32
  , privateKey_fromNormalBytes
  , publicKey_fromBytes
  ) as Csl
import Contract.Prim.ByteArray (RawBytes)
import Ctl.Internal.Cardano.Types.Transaction
  ( PrivateKey(PrivateKey)
  , PublicKey(PublicKey)
  )
import Ctl.Internal.Cardano.Types.Transaction (mkEd25519Signature) as X
import Ctl.Internal.Deserialization.Keys (publicKeyFromBech32) as Csl
import Ctl.Internal.Types.Aliases (Bech32String)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Safe.Coerce (coerce)

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes =
  map wrap <<< toMaybe <<< Csl.privateKey_fromNormalBytes <<< unwrap

privateKeyFromBech32 :: Bech32String -> Maybe PrivateKey
privateKeyFromBech32 =
  coerce $ toMaybe <<< Csl.privateKey_fromBech32

publicKeyFromBytes :: RawBytes -> Maybe PublicKey
publicKeyFromBytes =
  map wrap <<< toMaybe <<< Csl.publicKey_fromBytes <<< unwrap

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 =
  coerce Csl.publicKeyFromBech32
