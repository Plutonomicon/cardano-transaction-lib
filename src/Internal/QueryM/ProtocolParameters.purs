-- | A module to get protocol parameters via Ogmios request
module Ctl.Internal.QueryM.ProtocolParameters
  ( getProtocolParameters
  , module GetProtocolParametersAff
  ) where

import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM (getProtocolParametersAff) as GetProtocolParametersAff
import Ctl.Internal.QueryM.Ogmios
  ( ProtocolParameters
  , queryProtocolParametersCall
  )
import Prelude (unit)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
