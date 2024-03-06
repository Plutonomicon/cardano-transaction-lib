module Ctl.Internal.Lens
  ( _witnessSet
  , _redeemers
  , _vkeys
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  ) where

import Prelude

import Cardano.Types
  ( Coin(..)
  , Redeemer
  , Transaction
  , TransactionBody
  , TransactionInput(..)
  , TransactionOutput(..)
  , TransactionWitnessSet
  , Vkeywitness
  )
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))

_body :: Lens' Transaction TransactionBody
_body = _Newtype <<< prop (Proxy :: Proxy "body")

_collateral :: Lens' TransactionBody (Array TransactionInput)
_collateral = _Newtype <<< prop (Proxy :: Proxy "collateral")

_collateralReturn :: Lens' TransactionBody (Maybe TransactionOutput)
_collateralReturn = _Newtype <<< prop (Proxy :: Proxy "collateralReturn")

_totalCollateral :: Lens' TransactionBody (Maybe Coin)
_totalCollateral = _Newtype <<< prop (Proxy :: Proxy "totalCollateral")

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = _Newtype <<< prop (Proxy :: Proxy "witnessSet")

_redeemers :: Lens' TransactionWitnessSet (Array Redeemer)
_redeemers = _Newtype <<< prop (Proxy :: Proxy "redeemers")

_vkeys :: Lens' TransactionWitnessSet (Array Vkeywitness)
_vkeys = _Newtype <<< prop (Proxy :: Proxy "vkeys")
