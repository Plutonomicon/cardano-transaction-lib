module Ctl.Internal.Deserialization.Keys
  ( publicKeyFromBech32
  , privateKeyFromBytes
  , ed25519SignatureFromBech32
  , privateKeyToBech32
  , privateKeyFromBech32
  , freshPrivateKey
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
import Effect (Effect)

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = _publicKeyFromBech32 maybeFfiHelper

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = _privateKeyFromBytes maybeFfiHelper

ed25519SignatureFromBech32 :: Bech32String -> Maybe Ed25519Signature
ed25519SignatureFromBech32 = _ed25519SignatureFromBech32 maybeFfiHelper

privateKeyFromBech32 :: Bech32String -> Maybe PrivateKey
privateKeyFromBech32 = _privateKeyFromBech32 maybeFfiHelper

foreign import freshPrivateKey
  :: Effect PrivateKey

foreign import _ed25519SignatureFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe Ed25519Signature

foreign import _publicKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PublicKey

foreign import _privateKeyFromBytes
  :: MaybeFfiHelper -> RawBytes -> Maybe PrivateKey

foreign import privateKeyToBech32 :: PrivateKey -> Bech32String

foreign import _privateKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PrivateKey
