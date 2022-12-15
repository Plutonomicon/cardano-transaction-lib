module Ctl.Internal.QueryM.Config
  ( testnetTraceQueryConfig
  , testnetQueryConfig
  ) where

import Ctl.Internal.QueryM (QueryConfig, emptyHooks)
import Ctl.Internal.QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
  )
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId))
import Data.Log.Level (LogLevel(Error, Trace))
import Data.Maybe (Maybe(Nothing))

testnetTraceQueryConfig :: QueryConfig
testnetTraceQueryConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , kupoConfig: defaultKupoServerConfig
  , networkId: TestnetId
  , logLevel: Trace
  , walletSpec: Nothing
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  }

testnetQueryConfig :: QueryConfig
testnetQueryConfig = testnetTraceQueryConfig { logLevel = Error }
