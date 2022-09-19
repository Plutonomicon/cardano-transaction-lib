module Deserialization.Keys
  ( publicKeyFromBech32
  , privateKeyFromBytes
  ) where

import Types.Aliases (Bech32String)
import Data.Maybe (Maybe)
import Serialization.Types (PublicKey, PrivateKey)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Types.RawBytes (RawBytes)

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = _publicKeyFromBech32 maybeFfiHelper

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = _privateKeyFromBytes maybeFfiHelper

foreign import _publicKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PublicKey

foreign import _privateKeyFromBytes
  :: MaybeFfiHelper -> RawBytes -> Maybe PrivateKey
