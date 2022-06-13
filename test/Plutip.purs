module Test.Plutip where

import Prelude

import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(..))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)
import Plutip.Server (startPlutipCluster, stopPlutipCluster)
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
  Utils.interpret suite

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
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig:
      { port: UInt.fromInt 1337
      , host: "localhost"
      , secure: false
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 9999
      , host: "localhost"
      , secure: false
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8081
      , host: "localhost"
      , secure: false
      }
  }

-- Requires `plutip-server` running
suite :: TestPlanM Unit
suite = do
  group "Plutip" do
    test "startPlutipCluster" do
      startRes <- startPlutipCluster config
      startRes `shouldSatisfy` case _ of
        ClusterStartupSuccess _ -> true
        _ -> false
      liftEffect $ Console.log $ show startRes
      stopRes <- stopPlutipCluster config
      stopRes `shouldSatisfy` case _ of
        StopClusterSuccess -> true
        _ -> false
      liftEffect $ Console.log $ show stopRes
