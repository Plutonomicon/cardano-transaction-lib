module Ctl.Internal.Contract where

import Prelude

import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters) as Ogmios
import Ctl.Internal.Types.Chain (Tip)
import Effect.Aff.Class (liftAff)

getChainTip :: Contract Tip
getChainTip = do
  queryHandle <- getQueryHandle
  liftAff $ queryHandle.getChainTip

-- | Returns the `ProtocolParameters` from the environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: Contract Ogmios.ProtocolParameters
getProtocolParameters =
  asks $ _.ledgerConstants >>> _.pparams

