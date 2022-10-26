module Contract.ProtocolParameters
  ( getProtocolParameters
  ) where

import Contract.Monad (Contract, wrapContract)
import Ctl.Internal.QueryM.Ogmios (ProtocolParameters)
import Ctl.Internal.QueryM.ProtocolParameters (askProtocolParameters) as QueryM

getProtocolParameters :: forall (r :: Row Type). Contract r ProtocolParameters
getProtocolParameters = wrapContract QueryM.askProtocolParameters
