module Serialization.Keys
  ( bytesFromPrivateKey
  , bytesFromPublicKey
  , privateKeyFromBytes
  , publicKeyFromBech32
  , publicKeyFromBytes
  , bech32FromPublicKey

  ) where

import Data.Maybe (Maybe)
import Effect (Effect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Types (PublicKey, PrivateKey)
import Types.Aliases (Bech32String)
import Types.RawBytes (RawBytes)

publicKeyFromBech32 :: Bech32String -> Maybe PublicKey
publicKeyFromBech32 = _publicKeyFromBech32 maybeFfiHelper

bech32FromPublicKey :: PublicKey -> Bech32String
bech32FromPublicKey = _bech32FromPublicKey maybeFfiHelper

bytesFromPublicKey :: PublicKey -> Maybe RawBytes
bytesFromPublicKey = _bytesFromPublicKey maybeFfiHelper

privateKeyFromBytes :: RawBytes -> Maybe PrivateKey
privateKeyFromBytes = _privateKeyFromBytes maybeFfiHelper

publicKeyFromBytes :: RawBytes -> Maybe PublicKey
publicKeyFromBytes = _publicKeyFromBytes maybeFfiHelper

bytesFromPrivateKey :: PrivateKey -> Maybe RawBytes
bytesFromPrivateKey = _bytesFromPrivateKey maybeFfiHelper

foreign import _publicKeyFromBech32
  :: MaybeFfiHelper -> Bech32String -> Maybe PublicKey

foreign import publicKeyFromPrivateKey
  :: PrivateKey -> Effect PublicKey

foreign import _privateKeyFromBytes
  :: MaybeFfiHelper -> RawBytes -> Maybe PrivateKey

foreign import _publicKeyFromBytes
  :: MaybeFfiHelper -> RawBytes -> Maybe PublicKey

foreign import _bytesFromPrivateKey
  :: MaybeFfiHelper -> PrivateKey -> Maybe RawBytes

foreign import _bytesFromPublicKey
  :: MaybeFfiHelper -> PublicKey -> Maybe RawBytes

foreign import _bech32FromPublicKey
  :: MaybeFfiHelper -> PublicKey -> Bech32String
