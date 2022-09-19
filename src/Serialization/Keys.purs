module Serialization.Keys
  ( bytesFromPrivateKey
  , bytesFromPublicKey
  , bech32FromPublicKey
  , bech32FromPrivateKey
  ) where

import Data.Maybe (Maybe)
import Effect (Effect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Serialization.Types (PublicKey, PrivateKey)
import Types.Aliases (Bech32String)
import Types.RawBytes (RawBytes)

bech32FromPublicKey :: PublicKey -> Bech32String
bech32FromPublicKey = _bech32FromPublicKey maybeFfiHelper

bech32FromPrivateKey :: PrivateKey -> Bech32String
bech32FromPrivateKey = _bech32FromPrivateKey maybeFfiHelper

bytesFromPrivateKey :: PrivateKey -> Maybe RawBytes
bytesFromPrivateKey = _bytesFromPrivateKey maybeFfiHelper

bytesFromPublicKey :: PublicKey -> Maybe RawBytes
bytesFromPublicKey = _bytesFromPublicKey maybeFfiHelper

foreign import publicKeyFromPrivateKey
  :: PrivateKey -> Effect PublicKey

foreign import _bytesFromPrivateKey
  :: MaybeFfiHelper -> PrivateKey -> Maybe RawBytes

foreign import _bytesFromPublicKey
  :: MaybeFfiHelper -> PublicKey -> Maybe RawBytes

foreign import _bech32FromPublicKey
  :: MaybeFfiHelper -> PublicKey -> Bech32String

foreign import _bech32FromPrivateKey
  :: MaybeFfiHelper -> PrivateKey -> Bech32String
