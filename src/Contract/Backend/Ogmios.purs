-- | Module for backend-specific functions that only work with Ogmios/Kupo backends
module Contract.Backend.Ogmios
  ( getPoolParameters
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Transaction (PoolPubKeyHash)
import Ctl.Internal.Cardano.Types.Transaction (PoolRegistrationParams)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.QueryM.Pools as QueryM

-- | **This function can only run with Ogmios backend**
-- |
-- | Blockfrost does not support fetching the required data:
-- | https://github.com/blockfrost/blockfrost-backend-ryo/issues/82
getPoolParameters
  :: PoolPubKeyHash
  -> Contract PoolRegistrationParams
getPoolParameters = wrapQueryM <<< QueryM.getPoolParameters
