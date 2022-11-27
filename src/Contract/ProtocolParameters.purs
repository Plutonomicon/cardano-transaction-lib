module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import Contract.Monad (Contract)
import Ctl.Internal.Contract (getProtocolParameters) as Contract
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters)

-- | Returns the `ProtocolParameters` from the `Contract` environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: Contract ProtocolParameters
getProtocolParameters = Contract.getProtocolParameters
