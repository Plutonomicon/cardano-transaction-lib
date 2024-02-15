-- | This module contains everything needed for `Contract` testing in Plutip
-- | environment.
module Contract.Test.Plutip
  ( PlutipTest
  , PlutipTestPlan
  , defaultPlutipConfig
  , module X
  ) where

import Contract.Monad (runContractInEnv) as X
import Contract.Wallet (withKeyWallet) as X
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Plutip.Server
  ( runPlutipContract
  , runPlutipTestPlan
  , testPlutipContracts
  , withPlutipContractEnv
  ) as X
import Ctl.Internal.Plutip.Types (PlutipConfig)
import Ctl.Internal.Plutip.Types (PlutipConfig) as X
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.ContractTest (ContractTestPlan) as Server
import Ctl.Internal.Test.ContractTest
  ( noWallet
  , sameWallets
  , withWallets
  ) as X
import Ctl.Internal.Test.UtxoDistribution
  ( class UtxoDistribution
  , InitialUTxODistribution
  , InitialUTxOs
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , UtxoAmount
  , withStakeKey
  ) as X
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt

-- | Type synonym for backwards compatibility.
type PlutipTest = ContractTest
type PlutipTestPlan = Server.ContractTestPlan

-- | A default value for `PlutipConfig` type.
defaultPlutipConfig :: PlutipConfig
defaultPlutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , suppressLogs: true
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.1
      , epochSize: Nothing
      , maxTxSize: Nothing
      , raiseExUnitsToMax: false
      }
  }
