-- | A module to get "currentEpoch" via an Ogmios request.
module QueryM.CurrentEpoch
  ( getCurrentEpoch
  ) where

import Prelude
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (CurrentEpochQR, queryCurrentEpochCall) as Ogmios

getCurrentEpoch :: QueryM Ogmios.CurrentEpochQR
getCurrentEpoch =
  mkOgmiosRequest Ogmios.queryCurrentEpochCall _.currentEpoch unit
