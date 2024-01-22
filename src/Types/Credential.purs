module Cardano.Types.Credential where


import Prelude

import Cardano.Types.ByronAddress (ByronAddress)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash(..))
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.ScriptHash (ScriptHash(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Credential = PubKeyHashCredential Ed25519KeyHash | ScriptHashCredential ScriptHash

derive instance Generic Credential _
derive instance Eq Credential
derive instance Ord Credential

instance Show Credential where
  show = genericShow
