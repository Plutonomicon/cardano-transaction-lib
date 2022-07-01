-- | Requires `plutip-server` running, as well as `plutip-runtime`.
-- | `plutip-server` PR:
-- | https://github.com/mlabs-haskell/plutip/pull/79 (run with `cabal run plutip-server`)
-- |
-- | For `plutip-runtime`, run from the root directory of the repo:
-- |
-- | ```
-- | nix run -L .#plutip-runtime
-- | ```
module Test.Plutip where

import Prelude

import Contract.Chain (getTip)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(..))
import Data.Maybe (Maybe(..))
import Data.UInt as UInt
import Control.Monad.Error.Class (withResource)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)
import Plutip.Server
  ( liftContract
  , runPlutipM
  , startPlutipCluster
  , startPlutipServer
  , stopChildProcess
  , stopPlutipCluster
  )
import Plutip.Types
  ( PlutipConfig
  , StartClusterResponse(..)
  , StopClusterResponse(..)
  )
import Test.Spec.Assertions (shouldSatisfy)
import Test.Utils as Utils
import TestM (TestPlanM)

-- Run with `spago test --main Test.Plutip`
main :: Effect Unit
main = launchAff_ do
  Utils.interpretWithTimeout Nothing suite

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , distribution:
      [ [ BigInt.fromInt 1000000000
        , BigInt.fromInt 2000000000
        ]
      , [ BigInt.fromInt 2000000000 ]
      ]
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

suite :: TestPlanM Unit
suite = do
  group "Plutip" do
    test "startPlutipCluster / stopPlutipCluster" do
      withResource (startPlutipServer config) stopChildProcess $ const do
        startRes <- startPlutipCluster config
        startRes `shouldSatisfy` case _ of
          ClusterStartupSuccess _ -> true
          _ -> false
        liftEffect $ Console.log $ "startPlutipCluster: " <> show startRes
        stopRes <- stopPlutipCluster config
        stopRes `shouldSatisfy` case _ of
          StopClusterSuccess -> true
          _ -> false
        liftEffect $ Console.log $ "stopPlutipCluster: " <> show stopRes
    test "runPlutipM" do
      runPlutipM config $ liftContract do
        ct <- getTip
        liftEffect $ Console.log $ show $ ct
