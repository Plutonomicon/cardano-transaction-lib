module Ctl.Internal.Types.PaymentPubKey
  ( PaymentPubKey(PaymentPubKey)
  , paymentPubKeyToRequiredSigner
  , paymentPubKeyToVkey
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( PublicKey
  , RequiredSigner(RequiredSigner)
  , Vkey(Vkey)
  , convertPubKey
  )
import Ctl.Internal.Serialization (publicKeyHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- Plutus has a type called `PubKey` which we replace with `PublicKey`
newtype PaymentPubKey = PaymentPubKey PublicKey

derive instance Generic PaymentPubKey _
derive instance Newtype PaymentPubKey _
derive newtype instance Eq PaymentPubKey
derive newtype instance Ord PaymentPubKey

instance Show PaymentPubKey where
  show = genericShow

paymentPubKeyToVkey :: PaymentPubKey -> Vkey
paymentPubKeyToVkey (PaymentPubKey pk) = Vkey pk

paymentPubKeyToRequiredSigner :: PaymentPubKey -> RequiredSigner
paymentPubKeyToRequiredSigner (PaymentPubKey pk) =
  RequiredSigner <<< publicKeyHash $ convertPubKey pk
