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
  , module Contract.Address
  , module Contract.Monad
  , module Data.Log.Level
  , module Data.Log.Message
  , module Serialization
  , module QueryM.ServerConfig
  , module Wallet.Spec
  , module Wallet.Key
  ) where

import Prelude

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Contract.Monad (ConfigParams)
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))
import QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Serialization (privateKeyFromBytes)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      )
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )

testnetConfig :: ConfigParams ()
testnetConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , ctlServerConfig: Just defaultServerConfig
  , networkId: TestnetId
  , extraConfig: {}
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: mempty
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
