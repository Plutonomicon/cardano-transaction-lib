-- | Exposes some pre-defined Contract configurations. Re-exports all modules needed to modify `ConfigParams`.
module CTL.Contract.Config
  ( testnetConfig
  , testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetLodeConfig
  , mainnetConfig
  , mainnetNamiConfig
  , mainnetGeroConfig
  , module CTL.Contract.Address
  , module CTL.Contract.Monad
  , module Data.Log.Level
  , module Data.Log.Message
  , module CTL.Internal.Serialization
  , module CTL.Internal.QueryM.ServerConfig
  , module CTL.Internal.Wallet.Spec
  , module CTL.Internal.Wallet.Key
  ) where

import CTL.Contract.Address (NetworkId(MainnetId, TestnetId))
import CTL.Contract.Monad (ConfigParams)
import CTL.Internal.QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  )
import CTL.Internal.Serialization (privateKeyFromBytes)
import CTL.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import CTL.Internal.Wallet.Spec
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
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
  , ctlServerConfig: Just defaultServerConfig
  , networkId: TestnetId
  , extraConfig: {}
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  }

testnetNamiConfig :: ConfigParams ()
testnetNamiConfig = testnetConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ConfigParams ()
testnetGeroConfig = testnetConfig { walletSpec = Just ConnectToGero }

testnetFlintConfig :: ConfigParams ()
testnetFlintConfig = testnetConfig { walletSpec = Just ConnectToFlint }

testnetLodeConfig :: ConfigParams ()
testnetLodeConfig = testnetConfig { walletSpec = Just ConnectToLode }

mainnetConfig :: ConfigParams ()
mainnetConfig = testnetConfig { networkId = MainnetId }

mainnetNamiConfig :: ConfigParams ()
mainnetNamiConfig = mainnetConfig { walletSpec = Just ConnectToNami }

mainnetGeroConfig :: ConfigParams ()
mainnetGeroConfig = mainnetConfig { walletSpec = Just ConnectToGero }
