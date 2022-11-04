module Ctl.Internal.QueryM.Config
  ( testnetTraceQueryConfig
  , testnetQueryConfig
  ) where

import Prelude

import Ctl.Internal.QueryM (QueryConfig)
import Ctl.Internal.QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId))
import Data.Log.Level (LogLevel(Error, Trace))
import Data.Maybe (Maybe(Just, Nothing))

testnetTraceQueryConfig :: QueryConfig
testnetTraceQueryConfig =
  { ctlServerConfig: Just defaultServerConfig
  , ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , networkId: TestnetId
  , logLevel: Trace
  , walletSpec: Nothing
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: mempty
  }

testnetQueryConfig :: QueryConfig
testnetQueryConfig = testnetTraceQueryConfig { logLevel = Error }
