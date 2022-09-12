module QueryM.NetworkId
  ( getNetworkId
  ) where

import Prelude

import Control.Monad.Reader (asks)
import QueryM (QueryM)
import Serialization.Address (NetworkId)

getNetworkId :: QueryM NetworkId
getNetworkId = asks $ _.config >>> _.networkId
