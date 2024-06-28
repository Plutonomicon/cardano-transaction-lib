module Contract.Test.Testnet
  ( defaultTestnetConfig
  ) where

import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Testnet.Types (Era(Babbage), TestnetConfig)
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
      , era: Babbage
      , slotLength: Seconds 0.1 -- FIXME
      , epochSize: Nothing -- FIXME
      , maxTxSize: Nothing -- FIXME
      , raiseExUnitsToMax: false -- FIXME
      }
  }
