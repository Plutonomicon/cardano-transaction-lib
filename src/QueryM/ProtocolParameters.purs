-- | A module to get protocol parameters via Ogmios request
module QueryM.ProtocolParameters
  ( getProtocolParameters
  ) where

import Prelude

import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (queryProtocolParametersCall, ProtocolParameters)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
