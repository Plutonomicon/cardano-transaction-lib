module Serialization.Keys
  ( bytesFromPrivateKey
  , bytesFromPublicKey
  , bech32FromPublicKey
  , bech32FromPrivateKey
  ) where

import Effect (Effect)
import Serialization.Types (PublicKey, PrivateKey)
import Types.Aliases (Bech32String)
import Types.RawBytes (RawBytes)

bech32FromPublicKey :: PublicKey -> Bech32String
bech32FromPublicKey = _bech32FromPublicKey

bech32FromPrivateKey :: PrivateKey -> Bech32String
bech32FromPrivateKey = _bech32FromPrivateKey

foreign import publicKeyFromPrivateKey
  :: PrivateKey -> Effect PublicKey

foreign import bytesFromPrivateKey
  :: PrivateKey -> RawBytes

foreign import bytesFromPublicKey
  :: PublicKey -> RawBytes

foreign import _bech32FromPublicKey
  :: PublicKey -> Bech32String

foreign import _bech32FromPrivateKey
  :: PrivateKey -> Bech32String
