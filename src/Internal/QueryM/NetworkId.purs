module CTL.Internal.QueryM.NetworkId
  ( getNetworkId
  ) where

import Prelude

import CTL.Internal.QueryM (QueryM)
import CTL.Internal.Serialization.Address (NetworkId)
import Control.Monad.Reader (asks)

getNetworkId :: QueryM NetworkId
getNetworkId = asks $ _.config >>> _.networkId
