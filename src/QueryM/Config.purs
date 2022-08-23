module QueryM.Config
  ( testnetTraceQueryConfig
  , testnetQueryConfig
  ) where

import Data.Log.Level (LogLevel(Error, Trace))
import Data.Maybe (Maybe(Nothing))
import QueryM (QueryConfig)
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Serialization.Address (NetworkId(TestnetId))

testnetTraceQueryConfig :: QueryConfig
testnetTraceQueryConfig =
  { ctlServerConfig: defaultServerConfig
  , ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , networkId: TestnetId
  , logLevel: Trace
  , walletSpec: Nothing
  , customLogger: Nothing
  , suppressLogs: false
  }

testnetQueryConfig :: QueryConfig
testnetQueryConfig = testnetTraceQueryConfig { logLevel = Error }
