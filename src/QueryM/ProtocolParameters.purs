-- | A module to get protocol parameters via Ogmios request
module QueryM.ProtocolParameters
  ( getProtocolParameters
  , module GetProtocolParametersAff
  ) where

import Prelude (unit)

import QueryM (QueryM, mkOgmiosRequest)
import QueryM (getProtocolParametersAff) as GetProtocolParametersAff
import QueryM.Ogmios (queryProtocolParametersCall, ProtocolParameters)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
