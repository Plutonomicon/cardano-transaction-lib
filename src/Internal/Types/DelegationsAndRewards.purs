module Ctl.Internal.Types.DelegationsAndRewards
  ( DelegationsAndRewards
  ) where

import Data.Maybe

import Ctl.Internal.Cardano.Types.Transaction (PoolPubKeyHash)
import Ctl.Internal.Cardano.Types.Value (Coin)

type DelegationsAndRewards =
  { rewards :: Maybe Coin
  , delegate :: Maybe PoolPubKeyHash
  }
