-- | A module for wallet/backend synchronization control.
-- | See `doc/query-layers.md`.
module Contract.Sync
  ( module X
  , withoutSync
  ) where

import Contract.Config (disabledSynchronizationParams)
import Contract.Monad (Contract)
import Control.Monad.Reader (local)
import Ctl.Internal.BalanceTx.Sync
  ( syncBackendWithWallet
  , syncWalletWithTransaction
  , syncWalletWithTxInputs
  ) as X

-- | A helper to set `synchronizationParams` to `disabledSynchronizationParams`
-- | locally.
withoutSync :: forall (a :: Type). Contract a -> Contract a
withoutSync = do
  local \cfg -> cfg { synchronizationParams = disabledSynchronizationParams }
