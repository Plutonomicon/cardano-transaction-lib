module Contract.Crypto.SECP256k1
  ( SECP256k1PrivateKey(SECP256k1PrivateKey)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Noble.Secp256k1.ECDSA (PrivateKey)

newtype SECP256k1PrivateKey = SECP256k1PrivateKey PrivateKey

derive instance Newtype SECP256k1PrivateKey _

derive instance Generic SECP256k1PrivateKey _

instance Show SECP256k1PrivateKey where
  show = genericShow

derive newtype instance Eq SECP256k1PrivateKey
derive newtype instance Ord SECP256k1PrivateKey
