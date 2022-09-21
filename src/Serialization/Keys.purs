module Serialization.Keys
  ( bytesFromPrivateKey
  , bytesFromPublicKey
  , bech32FromPublicKey
  , bech32FromPrivateKey
  , bech32FromEd25519Signature
  ) where

import Effect (Effect)
import Serialization.Types (PublicKey, PrivateKey, Ed25519Signature)
import Types.Aliases (Bech32String)
import Types.RawBytes (RawBytes)

foreign import publicKeyFromPrivateKey
  :: PrivateKey -> Effect PublicKey

foreign import bytesFromPrivateKey
  :: PrivateKey -> RawBytes

foreign import bytesFromPublicKey
  :: PublicKey -> RawBytes

foreign import bech32FromPublicKey
  :: PublicKey -> Bech32String

foreign import bech32FromPrivateKey
  :: PrivateKey -> Bech32String

foreign import bech32FromEd25519Signature
  :: Ed25519Signature -> Bech32String
