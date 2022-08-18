-- | A module to get "currentEpoch" via an Ogmios request.
module QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude

import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (CurrentEpoch, queryCurrentEpochCall) as Ogmios

-- | Get the current Epoch. Details can be found https://ogmios.dev/api/ under
-- | "currentEpoch" query
getCurrentEpoch :: QueryM Ogmios.CurrentEpoch
getCurrentEpoch =
  mkOgmiosRequest Ogmios.queryCurrentEpochCall _.currentEpoch unit
