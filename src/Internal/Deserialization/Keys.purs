module Ctl.Internal.Deserialization.Keys
  ( publicKeyFromBech32
  , privateKeyFromBytes
  , ed25519SignatureFromBech32
  ) where

import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Serialization.Types
  ( Ed25519Signature
  , PrivateKey
  , PublicKey
  )
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Data.Maybe (Maybe)

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = _publicKeyFromBech32 maybeFfiHelper

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = _privateKeyFromBytes maybeFfiHelper

ed25519SignatureFromBech32 :: Bech32String -> Maybe Ed25519Signature
ed25519SignatureFromBech32 = _ed25519SignatureFromBech32 maybeFfiHelper

foreign import _ed25519SignatureFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe Ed25519Signature

foreign import _publicKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PublicKey

foreign import _privateKeyFromBytes
  :: MaybeFfiHelper -> RawBytes -> Maybe PrivateKey

