-- | A module to get "systemStart" via an Ogmios request.
module QueryM.SystemStart
  ( getSystemStart
  ) where

import Prelude
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (SystemStartQR, querySystemStartCall) as Ogmios

-- | Get the current system start time. Details can be found
-- | https://ogmios.dev/api/ under "systemStart" query
getSystemStart :: QueryM Ogmios.SystemStartQR
getSystemStart =
  mkOgmiosRequest Ogmios.querySystemStartCall _.systemStart unit
