module Scaffold.Api
  ( contractApi
  ) where

import Prelude

import Contract.Config (LogLevel(..), testnetNamiConfig)
import Contract.Monad (runContract)
import Contract.Prelude (Effect)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Contract.QueryBackend (mkBlockfrostBackendParams)
import Ctl.Internal.ServerConfig (blockfrostPublicPreprodServerConfig)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Scaffold as Scaffold

contractApi :: Effect (Promise Unit)
contractApi = fromAff $ runContract config $ Scaffold.contract
  where
  config = testnetNamiConfig { backendParams = bfParams, logLevel = Trace }
  bfParams = mkBlockfrostBackendParams
    { blockfrostConfig: blockfrostPublicPreprodServerConfig
    , blockfrostApiKey: Just "preprodAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    , confirmTxDelay: Just $ wrap 5.0
    }