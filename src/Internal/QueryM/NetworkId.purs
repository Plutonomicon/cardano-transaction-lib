module CTL.Internal.QueryM.NetworkId
  ( getNetworkId
  ) where

import Prelude

import Control.Monad.Reader (asks)
import CTL.Internal.QueryM (QueryM)
import CTL.Internal.Serialization.Address (NetworkId)

getNetworkId :: QueryM NetworkId
getNetworkId = asks $ _.config >>> _.networkId
