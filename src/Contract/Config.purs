-- | Exposes some pre-defined Contract configurations. Re-exports all modules needed to modify `ConfigParams`.
module Contract.Config
  ( testnetConfig
  , testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , mainnetConfig
  , mainnetNamiConfig
  , mainnetGeroConfig
  , mainnetFlintConfig
  , mainnetEternlConfig
  , mainnetLodeConfig
  , module Contract.Address
  , module Contract.Monad
  , module Data.Log.Level
  , module Data.Log.Message
  , module Ctl.Internal.Deserialization.Keys
  , module Ctl.Internal.QueryM.ServerConfig
  , module Ctl.Internal.Wallet.Spec
  , module Ctl.Internal.Wallet.Key
  , module X
  ) where

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Contract.Monad (ConfigParams)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.QueryM (emptyHooks)
import Ctl.Internal.QueryM (emptyHooks) as X
import Ctl.Internal.QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
  )
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Ctl.Internal.Wallet.Spec
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      )
  )
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))

testnetConfig :: ConfigParams ()
testnetConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , kupoConfig: defaultKupoServerConfig
  , networkId: TestnetId
  , extraConfig: {}
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  }

testnetNamiConfig :: ConfigParams ()
testnetNamiConfig = testnetConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ConfigParams ()
testnetGeroConfig = testnetConfig { walletSpec = Just ConnectToGero }

testnetFlintConfig :: ConfigParams ()
testnetFlintConfig = testnetConfig { walletSpec = Just ConnectToFlint }

testnetEternlConfig :: ConfigParams ()
testnetEternlConfig = testnetConfig { walletSpec = Just ConnectToEternl }

testnetLodeConfig :: ConfigParams ()
testnetLodeConfig = testnetConfig { walletSpec = Just ConnectToLode }

mainnetConfig :: ConfigParams ()
mainnetConfig = testnetConfig { networkId = MainnetId }

mainnetNamiConfig :: ConfigParams ()
mainnetNamiConfig = mainnetConfig { walletSpec = Just ConnectToNami }

mainnetGeroConfig :: ConfigParams ()
mainnetGeroConfig = mainnetConfig { walletSpec = Just ConnectToGero }

mainnetFlintConfig :: ConfigParams ()
mainnetFlintConfig = mainnetConfig { walletSpec = Just ConnectToFlint }

mainnetEternlConfig :: ConfigParams ()
mainnetEternlConfig = mainnetConfig { walletSpec = Just ConnectToEternl }

mainnetLodeConfig :: ConfigParams ()
mainnetLodeConfig = mainnetConfig { walletSpec = Just ConnectToLode }
