module Ctl.Internal.Contract (getChainTip, getProtocolParameters) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Contract.QueryHandle (getQueryHandle)
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters) as Ogmios
import Ctl.Internal.Types.Chain (Tip)
import Data.Either (either)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

getChainTip :: Contract Tip
getChainTip = do
  queryHandle <- getQueryHandle
  liftAff $
    queryHandle.getChainTip
      >>= either (liftEffect <<< throw <<< show) pure

-- | Returns the `ProtocolParameters` from the environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: Contract Ogmios.ProtocolParameters
getProtocolParameters =
  asks $ _.ledgerConstants >>> _.pparams

