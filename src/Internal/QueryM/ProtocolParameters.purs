-- | A module to get protocol parameters via Ogmios request
module CTL.Internal.QueryM.ProtocolParameters
  ( getProtocolParameters
  , module GetProtocolParametersAff
  ) where

import CTL.Internal.QueryM (QueryM, mkOgmiosRequest)
import CTL.Internal.QueryM (getProtocolParametersAff) as GetProtocolParametersAff
import CTL.Internal.QueryM.Ogmios
  ( ProtocolParameters
  , queryProtocolParametersCall
  )
import Prelude (unit)

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
