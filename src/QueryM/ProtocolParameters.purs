-- | A module to get protocol parameters via Ogmios request
module QueryM.ProtocolParameters
  ( getProtocolParameters
  ) where

import Prelude

import Data.Newtype (unwrap)
import QueryM (QueryM, mkOgmiosRequest)
import QueryM.Ogmios (queryProtocolParametersCall, ProtocolParameters)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters = unwrap <$>
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
