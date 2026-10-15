module Ctl.Internal.Contract (getChainTip, getProtocolParameters) where

import Prelude

import Cardano.Types.Chain (Tip)
import Cardano.Types.ProtocolParameters (ProtocolParameters)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.Monad (Contract, getProvider)
import Data.Either (either)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

getChainTip :: Contract Tip
getChainTip = do
  provider <- getProvider
  liftAff $
    provider.getChainTip
      >>= either (liftEffect <<< throw <<< show) pure

-- | Returns the `ProtocolParameters` from the environment.
-- | Note that this is not necessarily the current value from the ledger.
getProtocolParameters :: Contract ProtocolParameters
getProtocolParameters =
  asks $ _.ledgerConstants >>> _.pparams
