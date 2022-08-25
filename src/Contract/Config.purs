-- | Exposes some pre-defined Contract configurations. Re-exports all modules needed to modify `ConfigParams`.
module Contract.Config
  ( testnetConfig
  , testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
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

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Serialization (privateKeyFromBytes)
import Contract.Monad (ConfigParams)
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Maybe (Maybe(Just, Nothing))
import Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      )
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )
import QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Data.Log.Message (Message)

testnetConfig :: ConfigParams ()
testnetConfig =
  { ogmiosConfig: defaultOgmiosWsConfig
  , datumCacheConfig: defaultDatumCacheWsConfig
  , ctlServerConfig: Just defaultServerConfig
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

testnetFlintConfig :: ConfigParams ()
testnetFlintConfig = testnetConfig { walletSpec = Just ConnectToFlint }

testnetEternlConfig :: ConfigParams ()
testnetEternlConfig = testnetConfig { walletSpec = Just ConnectToEternl }

mainnetConfig :: ConfigParams ()
mainnetConfig = testnetConfig { networkId = MainnetId }

mainnetNamiConfig :: ConfigParams ()
mainnetNamiConfig = mainnetConfig { walletSpec = Just ConnectToNami }

mainnetGeroConfig :: ConfigParams ()
mainnetGeroConfig = mainnetConfig { walletSpec = Just ConnectToGero }
