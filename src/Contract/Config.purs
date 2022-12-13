-- | Exposes some pre-defined Contract configurations. Re-exports all modules needed to modify `ContractParams`.
module Contract.Config
  ( testnetConfig
  , testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  , mainnetConfig
  , mainnetNamiConfig
  , mainnetGeroConfig
  , mainnetFlintConfig
  , mainnetEternlConfig
  , mainnetLodeConfig
  , mainnetNuFiConfig
  , module Contract.Address
  , module Ctl.Internal.Contract.Monad
  , module Ctl.Internal.Contract.QueryBackend
  , module Data.Log.Level
  , module Data.Log.Message
  , module Ctl.Internal.Deserialization.Keys
  , module Ctl.Internal.QueryM.ServerConfig
  , module Ctl.Internal.Wallet.Spec
  , module Ctl.Internal.Wallet.Key
  , module X
  ) where

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Ctl.Internal.Contract.Hooks (Hooks, emptyHooks) as X
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend
  ( QueryBackendParams(CtlBackendParams, BlockfrostBackendParams)
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  )
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
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
      , ConnectToNuFi
      )
  )
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))

testnetConfig :: ContractParams
testnetConfig =
  { backendParams: mkCtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig
      , kupoConfig: defaultKupoServerConfig
      }
  , ctlServerConfig: Just defaultServerConfig
  , networkId: TestnetId
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  }

testnetNamiConfig :: ContractParams
testnetNamiConfig = testnetConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ContractParams
testnetGeroConfig = testnetConfig { walletSpec = Just ConnectToGero }

testnetFlintConfig :: ContractParams
testnetFlintConfig = testnetConfig { walletSpec = Just ConnectToFlint }

testnetEternlConfig :: ContractParams
testnetEternlConfig = testnetConfig { walletSpec = Just ConnectToEternl }

testnetLodeConfig :: ContractParams
testnetLodeConfig = testnetConfig { walletSpec = Just ConnectToLode }

testnetNuFiConfig :: ContractParams
testnetNuFiConfig = testnetConfig { walletSpec = Just ConnectToNuFi }

mainnetConfig :: ContractParams
mainnetConfig = testnetConfig { networkId = MainnetId }

mainnetNamiConfig :: ContractParams
mainnetNamiConfig = mainnetConfig { walletSpec = Just ConnectToNami }

mainnetGeroConfig :: ContractParams
mainnetGeroConfig = mainnetConfig { walletSpec = Just ConnectToGero }

mainnetFlintConfig :: ContractParams
mainnetFlintConfig = mainnetConfig { walletSpec = Just ConnectToFlint }

mainnetEternlConfig :: ContractParams
mainnetEternlConfig = mainnetConfig { walletSpec = Just ConnectToEternl }

mainnetLodeConfig :: ContractParams
mainnetLodeConfig = mainnetConfig { walletSpec = Just ConnectToLode }

mainnetNuFiConfig :: ContractParams
mainnetNuFiConfig = mainnetConfig { walletSpec = Just ConnectToNuFi }
