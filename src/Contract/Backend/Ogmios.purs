-- | Module for backend-specific functions that only work with Ogmios/Kupo backends
module Contract.Backend.Ogmios
  ( getPoolParameters
  , submitTxE
  ) where

import Prelude

import Cardano.Types (PoolParams, PoolPubKeyHash)
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.TransactionHash (TransactionHash)
import Contract.Monad (Contract)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.QueryM.Ogmios (submitTxOgmios) as Ogmios
import Ctl.Internal.QueryM.Ogmios.Types (SubmitTxR)
import Ctl.Internal.QueryM.Pools (getPoolParameters) as QueryM

-- | **This function can only run with Ogmios backend**
-- |
-- | Blockfrost does not support fetching the required data:
-- | https://github.com/blockfrost/blockfrost-backend-ryo/issues/82
getPoolParameters
  :: PoolPubKeyHash
  -> Contract PoolParams
getPoolParameters = wrapQueryM <<< QueryM.getPoolParameters

-- | Error returning variant
submitTxE :: TransactionHash -> CborBytes -> Contract SubmitTxR
submitTxE txhash cbor = wrapQueryM $ Ogmios.submitTxOgmios txhash cbor
