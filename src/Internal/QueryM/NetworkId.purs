module Ctl.Internal.QueryM.NetworkId
  ( getNetworkId
  ) where

import Prelude

import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.Serialization.Address (NetworkId)
import Control.Monad.Reader (asks)

getNetworkId :: QueryM NetworkId
getNetworkId = asks $ _.config >>> _.networkId
