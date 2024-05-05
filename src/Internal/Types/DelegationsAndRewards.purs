module Ctl.Internal.Types.DelegationsAndRewards
  ( DelegationsAndRewards
  ) where

import Data.Maybe

import Cardano.Types.Coin (Coin)
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)

type DelegationsAndRewards =
  { rewards :: Maybe Coin
  , delegate :: Maybe PoolPubKeyHash
  }
