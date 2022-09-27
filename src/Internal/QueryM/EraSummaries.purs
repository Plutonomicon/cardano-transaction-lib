-- | A module to get "eraSummaries" via an Ogmios request.
module Ctl.Internal.QueryM.EraSummaries
  ( getEraSummaries
  ) where

import Prelude

import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios (EraSummaries, queryEraSummariesCall) as Ogmios

-- | Get `EraSummaries` as used for Slot arithemetic. Details can be found
-- | https://ogmios.dev/api/ under "eraSummaries" query
getEraSummaries :: QueryM Ogmios.EraSummaries
getEraSummaries =
  mkOgmiosRequest Ogmios.queryEraSummariesCall _.eraSummaries unit
