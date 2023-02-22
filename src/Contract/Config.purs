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
  , module Data.Log.Level
  , module Data.Log.Message
  , module Ctl.Internal.Deserialization.Keys
  , module Ctl.Internal.ServerConfig
  , module Ctl.Internal.Wallet.Spec
  , module Ctl.Internal.Wallet.Key
  , module X
  ) where

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Ctl.Internal.Contract.Hooks (Hooks, emptyHooks) as X
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.Monad (ContractParams)
import Ctl.Internal.Contract.QueryBackend
  ( BlockfrostBackendParams
  , CtlBackend
  , CtlBackendParams
  , QueryBackend(BlockfrostBackend, CtlBackend)
  , QueryBackendParams(BlockfrostBackendParams, CtlBackendParams)
  , defaultConfirmTxDelay
  , getBlockfrostBackend
  , getCtlBackend
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  ) as X
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.ServerConfig
  ( Host
  , ServerConfig
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
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
      , ConnectToNuFi
      )
  )
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Milliseconds(Milliseconds))

testnetConfig :: ContractParams
testnetConfig =
  { backendParams: mkCtlBackendParams
      { ogmiosConfig: defaultOgmiosWsConfig
      , kupoConfig: defaultKupoServerConfig
      }
  , networkId: TestnetId
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams:
      { syncWallet:
          { delay: Milliseconds 1_000.0, timeout: Milliseconds 100_000.0 }
      , syncBackend:
          { delay: Milliseconds 3_000.0, timeout: Milliseconds 100_000.0 }
      }
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
