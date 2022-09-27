module Ctl.Internal.QueryM.NetworkId
  ( getNetworkId
  ) where

import Prelude

import Control.Monad.Reader (asks)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.Serialization.Address (NetworkId)

getNetworkId :: QueryM NetworkId
getNetworkId = asks $ _.config >>> _.networkId
