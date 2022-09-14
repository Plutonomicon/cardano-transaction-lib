module CTL.Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import CTL.Contract.Monad (Contract, wrapContract)
import CTL.Internal.QueryM.Ogmios (ProtocolParameters)
import CTL.Internal.QueryM.ProtocolParameters (getProtocolParameters) as QueryM

getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.getProtocolParameters
