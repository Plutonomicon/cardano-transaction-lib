-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Scaffold.Main (main) where

import Contract.Prelude

import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Contract.Test.Plutip (PlutipTest, testPlutipContracts)
import Contract.Test.Plutip as Contract.Test.Plutip
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff (Milliseconds(Milliseconds))
import Mote (MoteT, test)
import Scaffold as Scaffold
import Test.Scaffold.Plan as Plan
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = Contract.Monad.launchAff_ $ do
  Plan.interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
    $ testPlutipContracts config suite

suite :: MoteT Aff PlutipTest Aff Unit
suite = do
  test "Print PubKey" do
    let
      distribution :: Contract.Test.Plutip.InitialUTxOs
      distribution =
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 2_000_000_000
        ]
    Contract.Test.Plutip.withWallets distribution \_ -> do
      Scaffold.contract

config :: Contract.Test.Plutip.PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig: Just
      { port: UInt.fromInt 8083
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
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: Contract.Config.emptyHooks
  }
