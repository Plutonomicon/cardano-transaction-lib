module Ctl.Internal.Lens (_witnessSet, _redeemers) where

import Prelude

import Cardano.Types (Redeemer, Transaction, TransactionWitnessSet)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = _Newtype <<< prop (Proxy :: Proxy "witnessSet")

_redeemers :: Lens' TransactionWitnessSet (Array Redeemer)
_redeemers = _Newtype <<< prop (Proxy :: Proxy "redeemers")
