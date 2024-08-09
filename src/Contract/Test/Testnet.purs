module Contract.Test.Testnet
  ( defaultTestnetConfig
  , module X
  ) where

import Contract.Monad (runContractInEnv) as X
import Contract.Wallet (withKeyWallet) as X
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Test.ContractTest
  ( ContractTest
  , ContractTestPlan(ContractTestPlan)
  , noWallet
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
import Ctl.Internal.Testnet.Contract
  ( runTestnetContract
  , runTestnetTestPlan
  , testTestnetContracts
  ) as X
import Ctl.Internal.Testnet.Types
  ( Era
      ( Byron
      , Shelley
      , Allegra
      , Mary
      , Alonzo
      , Babbage
      , Conway
      )
  , TestnetConfig
  ) as X
import Ctl.Internal.Testnet.Types (Era(Conway), TestnetConfig)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt

defaultTestnetConfig :: TestnetConfig
defaultTestnetConfig =
  { logLevel: Trace
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
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { testnetMagic: 2
      , era: Conway
      , slotLength: Seconds 0.1
      , epochSize: Nothing
      }
  }
