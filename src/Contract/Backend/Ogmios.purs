-- | Module for backend-specific functions that only work with Ogmios/Kupo backends
module Contract.Backend.Ogmios
  ( getPoolParameters
  , submitTxE
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (PoolPubKeyHash)
import Ctl.Internal.Cardano.Types.Transaction (PoolRegistrationParams)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.QueryM (submitTxOgmios) as QueryM
import Ctl.Internal.QueryM.Ogmios (SubmitTxR, TxHash)
import Ctl.Internal.QueryM.Pools (getPoolParameters) as QueryM
import Ctl.Internal.Types.CborBytes (CborBytes)

-- | **This function can only run with Ogmios backend**
-- |
-- | Blockfrost does not support fetching the required data:
-- | https://github.com/blockfrost/blockfrost-backend-ryo/issues/82
getPoolParameters
  :: PoolPubKeyHash
  -> Contract PoolRegistrationParams
getPoolParameters = wrapQueryM <<< QueryM.getPoolParameters

-- | Error returning variant
submitTxE :: TxHash -> CborBytes -> Contract SubmitTxR
submitTxE txhash cbor = wrapQueryM $ QueryM.submitTxOgmios txhash cbor
