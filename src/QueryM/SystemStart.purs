-- | A module to get "systemStart" via an Ogmios request.
module QueryM.SystemStart
  ( getSystemStart
  ) where

import Prelude
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (SystemStartQR, querySystemStartCall) as Ogmios

getSystemStart :: QueryM Ogmios.SystemStartQR
getSystemStart =
  mkOgmiosRequest Ogmios.querySystemStartCall _.systemStart unit
