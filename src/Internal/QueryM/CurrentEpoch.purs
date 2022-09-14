-- | A module to get "currentEpoch" via an Ogmios request.
module CTL.Internal.QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude
import CTL.Internal.QueryM (QueryM, mkOgmiosRequest)
import CTL.Internal.QueryM.Ogmios (CurrentEpoch, queryCurrentEpochCall) as Ogmios

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM Ogmios.CurrentEpoch
getCurrentEpoch =
  mkOgmiosRequest Ogmios.queryCurrentEpochCall _.currentEpoch unit
