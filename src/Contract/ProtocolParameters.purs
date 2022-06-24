module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import QueryM.ProtocolParameters (getProtocolParameters) as QueryM
import Contract.Monad (Contract, wrapContract)
import QueryM.Ogmios (ProtocolParameters)

getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.getProtocolParameters
