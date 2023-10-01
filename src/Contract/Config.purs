-- | Exposes some pre-defined Contract configurations. Re-exports all modules needed to modify `ContractParams`.
module Contract.Config
  ( testnetConfig
  , testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  , testnetLaceConfig
  , mainnetConfig
  , mainnetNamiConfig
  , mainnetGeroConfig
  , mainnetFlintConfig
  , mainnetEternlConfig
  , mainnetLodeConfig
  , mainnetNuFiConfig
  , mainnetLaceConfig
  , defaultSynchronizationParams
  , strictSynchronizationParams
  , defaultTimeParams
  , module Contract.Address
  , module Data.Log.Level
  , module Data.Log.Message
  , module Ctl.Internal.Deserialization.Keys
  , module Ctl.Internal.ServerConfig
  , module Ctl.Internal.Wallet.Spec
  , module Ctl.Internal.Wallet.Key
  , module X
  ) where

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Ctl.Internal.BalanceTx.Sync
  ( disabledSynchronizationParams
  ) as X
import Ctl.Internal.Contract.Hooks (Hooks, emptyHooks) as X
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.Monad
  ( ContractParams
  , ContractSynchronizationParams
  , ContractTimeParams
  )
import Ctl.Internal.Contract.Monad
  ( ContractParams
  , ContractSynchronizationParams
  , ContractTimeParams
  ) as X
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
  , mkSelfHostedBlockfrostBackendParams
  ) as X
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.ServerConfig
  ( Host
  , ServerConfig
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , blockfrostSelfHostedServerConfig
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
  )
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , MnemonicSource(MnemonicString, MnemonicFile)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , WalletSpec
      ( UseKeys
      , UseMnemonic
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToNuFi
      , ConnectToLace
      , ConnectToGenericCip30
      )
  )
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Number (infinity)
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds(Seconds))

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
  , timeParams: defaultTimeParams
  , synchronizationParams: defaultSynchronizationParams
  }

-- | - `syncWallet` specifies delay and timeout for `syncWalletWithTransaction`
-- | and `syncWalletWithTxInputs` synchronization primitives.
-- | See `doc/query-layers.md` for more info.
-- | - `syncBackend` specifies delay and timeout for `syncBackendWithWallet`
-- | synchronization primitive.
-- | See `doc/query-layers.md` for more info.
-- | - `awaitTxConfirmed` specifies default delay and timeout for
-- | `awaitTxConfirmed` call.
defaultTimeParams :: ContractTimeParams
defaultTimeParams =
  { syncWallet:
      -- As clarified in Eternl discord, they synchronize with the server every 2
      -- minutes, so 125 seconds would probably be enough.
      -- For other wallets, it is not very important
      { delay: Milliseconds 1_000.0, timeout: Seconds 125.0 }
  , syncBackend:
      -- Operations are costly, so the delay is 3 set to seconds
      { delay: Milliseconds 3_000.0, timeout: Seconds 120.0 }
  , awaitTxConfirmed:
      -- CIP-30 calls are cheap, so the delay can be just 1 second
      { delay: Milliseconds 1_000.0, timeout: Seconds infinity }
  , waitUntilSlot: { delay: Milliseconds 1_000.0 }
  }

-- | Default synchronization parameters with all synchronization primitives
-- | enabled. `errorOnTimeout` options are all set to `false`.
-- | See `doc/query-layers.md` for more info.
defaultSynchronizationParams :: ContractSynchronizationParams
defaultSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: false, beforeCip30Methods: true, beforeBalancing: true }
  , syncWalletWithTxInputs: { errorOnTimeout: false, beforeCip30Sign: true }
  , syncWalletWithTransaction:
      { errorOnTimeout: false, beforeTxConfirmed: true }
  }

-- | Synchronization parameters with all synchronization primitives enabled
-- | and `errorOnTimeout` values set to `true`.
-- | See `doc/query-layers.md` for more info.
strictSynchronizationParams :: ContractSynchronizationParams
strictSynchronizationParams =
  { syncBackendWithWallet:
      { errorOnTimeout: true, beforeCip30Methods: true, beforeBalancing: true }
  , syncWalletWithTxInputs: { errorOnTimeout: true, beforeCip30Sign: true }
  , syncWalletWithTransaction: { errorOnTimeout: true, beforeTxConfirmed: true }
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

testnetLaceConfig :: ContractParams
testnetLaceConfig = testnetConfig { walletSpec = Just ConnectToLace }

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

mainnetLaceConfig :: ContractParams
mainnetLaceConfig = mainnetConfig { walletSpec = Just ConnectToLace }
