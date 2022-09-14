module CTL.Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import CTL.Internal.QueryM.ProtocolParameters (getProtocolParameters) as QueryM
import CTL.Contract.Monad (Contract, wrapContract)
import CTL.Internal.QueryM.Ogmios (ProtocolParameters)

getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.getProtocolParameters
