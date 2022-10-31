module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import Contract.Monad (Contract, wrapContract)
import Ctl.Internal.QueryM (getProtocolParameters) as QueryM
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters)

-- | Returns the `ProtocolParameters` from the `Contract` environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.getProtocolParameters
