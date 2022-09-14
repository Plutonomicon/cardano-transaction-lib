-- | A module to get protocol parameters via Ogmios request
module CTL.Internal.QueryM.ProtocolParameters
  ( getProtocolParameters
  , module GetProtocolParametersAff
  ) where

import Prelude (unit)

import CTL.Internal.QueryM (QueryM, mkOgmiosRequest)
import CTL.Internal.QueryM (getProtocolParametersAff) as GetProtocolParametersAff
import CTL.Internal.QueryM.Ogmios (queryProtocolParametersCall, ProtocolParameters)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
