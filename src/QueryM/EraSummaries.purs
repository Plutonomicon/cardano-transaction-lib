-- | A module to get "eraSummaries" via an Ogmios request.
module QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (EraSummaries, queryEraSummariesCall) as Ogmios

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: QueryM Ogmios.EraSummaries
getEraSummaries =
  mkOgmiosRequest Ogmios.queryEraSummariesCall _.eraSummaries unit
