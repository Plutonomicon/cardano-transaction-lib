module Ctl.Internal.Contract
  ( getChainTip
  , getProtocolParametersImpl
  ) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.Monad (Contract, getQueryHandle)
import Ctl.Internal.Types.Chain (Tip)
import Ctl.Internal.Types.ProtocolParameters (ProtocolParameters)
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
getProtocolParametersImpl :: Contract ProtocolParameters
getProtocolParametersImpl =
  asks $ _.ledgerConstants >>> _.pparams

