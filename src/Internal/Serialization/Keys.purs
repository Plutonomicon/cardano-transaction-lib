module Ctl.Internal.Serialization.Keys
  ( bytesFromPrivateKey
  , bytesFromPublicKey
  , bech32FromPublicKey
  , bech32FromPrivateKey
  , bech32FromEd25519Signature
  , publicKeyFromPrivateKey
  ) where

import Ctl.Internal.Serialization.Types
  ( Ed25519Signature
  , PrivateKey
  , PublicKey
  )
import Ctl.Internal.Types.Aliases (Bech32String)
import Ctl.Internal.Types.RawBytes (RawBytes)

foreign import publicKeyFromPrivateKey
  :: PrivateKey -> PublicKey

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
