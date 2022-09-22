-- | A module to get "currentEpoch" via an Ogmios request.
module Ctl.Internal.QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude

import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios (CurrentEpoch, queryCurrentEpochCall) as Ogmios

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM Ogmios.CurrentEpoch
getCurrentEpoch =
  mkOgmiosRequest Ogmios.queryCurrentEpochCall _.currentEpoch unit
