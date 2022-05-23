-- | A module to get "eraSummaries" via an Ogmios request.
module QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (EraSummariesQR, queryEraSummariesCall) as Ogmios

getEraSummaries :: QueryM Ogmios.EraSummariesQR
getEraSummaries =
  mkOgmiosRequest Ogmios.queryEraSummariesCall _.eraSummaries unit
