-- | `plutip-server` PR:
-- | https://github.com/mlabs-haskell/plutip/pull/79 (run with `cabal run plutip-server`)
module Test.Plutip where

import Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Chain (getTip)
import Contract.Monad (liftedE, liftedM, logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Transaction (balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (withResource)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Nothing))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Mote (group, test)
import Plutip.Server
  ( runPlutipContract
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
        startRes <- startPlutipCluster config unit
        startRes `shouldSatisfy` case _ of
          ClusterStartupSuccess _ -> true
          _ -> false
        liftEffect $ Console.log $ "startPlutipCluster: " <> show startRes
        stopRes <- stopPlutipCluster config
        stopRes `shouldSatisfy` case _ of
          StopClusterSuccess -> true
          _ -> false
        liftEffect $ Console.log $ "stopPlutipCluster: " <> show stopRes
    test "runPlutipContract" do
      let
        distribution :: Array BigInt /\ Array BigInt
        distribution =
          [ BigInt.fromInt 1000000000
          , BigInt.fromInt 2000000000
          ] /\
            [ BigInt.fromInt 2000000000 ]
      runPlutipContract config distribution \(alice /\ bob) -> do
        ct <- getTip
        withKeyWallet alice do
          pure unit -- sign, balance, submit, etc.
        withKeyWallet bob do
          pure unit -- sign, balance, submit, etc.
        liftEffect $ Console.log $ show $ ct
    test "runPlutipContract: pkh2pkh" do
      let
        distribution :: Array BigInt
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      runPlutipContract config distribution \alice -> do
        ct <- getTip
        withKeyWallet alice do
          pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
          let
            constraints :: Constraints.TxConstraints Void Void
            constraints = Constraints.mustPayToPubKey pkh
              $ Value.lovelaceValueOf
              $ BigInt.fromInt 2_000_000

            lookups :: Lookups.ScriptLookups Void
            lookups = mempty
          ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
          bsTx <-
            liftedE $ balanceAndSignTxE ubTx
          txId <- submit bsTx
          logInfo' $ "Tx ID: " <> show txId
        liftEffect $ Console.log $ show $ ct
