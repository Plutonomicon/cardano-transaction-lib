module Cardano.Types.PaymentPubKeyHash where

import Prelude

import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash(Ed25519KeyHash))
import Ctl.Internal.FromData (class FromData)
import Ctl.Internal.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype PaymentPubKeyHash = PaymentPubKeyHash Ed25519KeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow
