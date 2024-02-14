module Cardano.Types.StakePubKeyHash where

import Prelude

import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash(Ed25519KeyHash))
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype StakePubKeyHash = StakePubKeyHash Ed25519KeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance FromData StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow
