-- | Exposes some pre-defined Contract configurations.
module Contract.Config where

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Contract.Monad (ConfigParams)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import QueryM.ServerConfig
  ( defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Wallet.Spec (WalletSpec(ConnectToGero, ConnectToNami))

testnetConfig :: ConfigParams ()
testnetConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , ctlServerConfig: defaultServerConfig
  , networkId: TestnetId
  , logLevel: Trace
  , extraConfig: {}
  , walletSpec: Nothing
  , customLogger: Nothing
  }

testnetNamiConfig :: ConfigParams ()
testnetNamiConfig = testnetConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ConfigParams ()
testnetGeroConfig = testnetConfig { walletSpec = Just ConnectToGero }

mainnetConfig :: ConfigParams ()
mainnetConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , ctlServerConfig: defaultServerConfig
  , networkId: MainnetId
  , logLevel: Trace
  , extraConfig: {}
  , walletSpec: Nothing
  , customLogger: Nothing
  }
