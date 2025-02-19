module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import Cardano.Types.ProtocolParameters (ProtocolParameters)
import Contract.Monad (Contract)
import Ctl.Internal.Contract (getProtocolParameters) as Contract

-- | Returns the `ProtocolParameters` from the `Contract` environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: Contract ProtocolParameters
getProtocolParameters = Contract.getProtocolParameters
