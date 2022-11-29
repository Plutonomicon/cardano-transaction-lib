-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Scaffold.Main (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipConfig
  , PlutipTest
  , testPlutipContracts
  , withWallets
  )
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff (Milliseconds(Milliseconds))
import Mote (test)
import Scaffold (contract)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = launchAff_ $ do
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
    testPlutipContracts config suite

suite :: TestPlanM PlutipTest Unit
suite = do
  test "Print PubKey" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 2_000_000_000
        ]
    withWallets distribution \_ -> do
      contract

config :: PlutipConfig
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
  , hooks: emptyHooks
  }
