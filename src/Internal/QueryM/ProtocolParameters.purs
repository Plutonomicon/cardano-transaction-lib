-- | A module to get protocol parameters via Ogmios request
module Ctl.Internal.QueryM.ProtocolParameters
  ( getProtocolParameters
  , askProtocolParameters
  , module GetProtocolParametersAff
  ) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM (getProtocolParametersAff) as GetProtocolParametersAff
import Ctl.Internal.QueryM.Ogmios
  ( ProtocolParameters
  , queryProtocolParametersCall
  )

askProtocolParameters :: QueryM ProtocolParameters
askProtocolParameters =
  asks $ _.runtime >>> _.pparams

getProtocolParameters :: QueryM ProtocolParameters
getProtocolParameters =
  mkOgmiosRequest queryProtocolParametersCall _.getProtocolParameters unit
