module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import Contract.Monad (Contract, wrapContract)
import QueryM.Ogmios (ProtocolParameters)
import QueryM.ProtocolParameters (getProtocolParameters) as QueryM

getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.getProtocolParameters
