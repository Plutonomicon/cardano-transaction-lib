module Contract.Crypto.Secp256k1
  ( Secp256k1PrivateKey(Secp256k1PrivateKey)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Noble.Secp256k1.ECDSA (PrivateKey)

newtype Secp256k1PrivateKey = Secp256k1PrivateKey PrivateKey

derive instance Newtype Secp256k1PrivateKey _

derive instance Generic Secp256k1PrivateKey _

instance Show Secp256k1PrivateKey where
  show = genericShow

derive newtype instance Eq Secp256k1PrivateKey
derive newtype instance Ord Secp256k1PrivateKey
