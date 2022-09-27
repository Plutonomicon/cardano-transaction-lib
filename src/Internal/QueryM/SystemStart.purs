-- | A module to get "systemStart" via an Ogmios request.
module Ctl.Internal.QueryM.SystemStart
  ( getSystemStart
  ) where

import Prelude

import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios (SystemStart, querySystemStartCall) as Ogmios

-- | Get the current system start time. Details can be found
-- | https://ogmios.dev/api/ under "systemStart" query
getSystemStart :: QueryM Ogmios.SystemStart
getSystemStart =
  mkOgmiosRequest Ogmios.querySystemStartCall _.systemStart unit
