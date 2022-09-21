module QueryM.Config
  ( testnetTraceQueryConfig
  , testnetQueryConfig
  ) where

import Data.Log.Level (LogLevel(Error, Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (mempty)
import QueryM (QueryConfig)
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Serialization.Address (NetworkId(TestnetId))

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
