module Test.Ctl.Integration (main, testPlan, stakingSuite) where

import Prelude

import Contract.Address (Ed25519KeyHash, StakePubKeyHash(StakePubKeyHash))
import Contract.Backend.Ogmios (getPoolParameters)
import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Staking (getPoolIds, getPubKeyHashDelegationsAndRewards)
import Contract.Test (ContractTest, noWallet)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.Time (getEraSummaries, getSystemStart)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, cancelWith, effectCanceler, launchAff)
import Effect.Class (liftEffect)
import Mote (group, skip, test)
import Mote.Monad (mapTest)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.BalanceTx.Collateral as Collateral
import Test.Ctl.BalanceTx.Time as BalanceTx.Time
import Test.Ctl.Fixtures (ed25519KeyHash1)
import Test.Ctl.Logging as Logging
import Test.Ctl.PrivateKey as PrivateKey
import Test.Ctl.QueryM.AffInterface as QueryM.AffInterface
import Test.Ctl.Types.Interval as Types.Interval
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)

-- Run with `spago test --main Test.Ctl.Integration`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 450_000.0, exit = true }
      testPlan

-- Requires external services listed in README.md
testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  mapTest runQueryM' QueryM.AffInterface.suite
  -- These tests depend on assumptions about testnet history.
  -- We disabled them during transition from `testnet` to `preprod` networks.
  -- https://github.com/Plutonomicon/cardano-transaction-lib/issues/945
  skip $ flip mapTest Types.Interval.suite \f -> runContract
    testnetConfig { suppressLogs = true }
    do
      eraSummaries <- getEraSummaries
      sysStart <- getSystemStart
      liftEffect $ f eraSummaries sysStart
  -- TODO enable Pools integration tests
  skip $ mapTest (runContract testnetConfig) do
    group "Pools" do
      test "get metadata for all pools" do
        poolIds <- getPoolIds
        void $ traverse getPoolParameters poolIds
  Collateral.suite
  PrivateKey.suite
  Logging.suite
  -- TODO enable BalanceTx.Time integration tests
  skip BalanceTx.Time.suite
  where
  runQueryM' =
    runContract (testnetConfig { suppressLogs = true }) <<< wrapQueryM

stakingSuite :: TestPlanM ContractTest Unit
stakingSuite = do
  group "Staking" do
    test "getPoolIds" do
      noWallet do
        void $ getPoolIds
    test "getPubKeyHashDelegationsAndRewards #1" do
      noWallet do
        res <- getPubKeyHashDelegationsAndRewards $ StakePubKeyHash $ wrap
          ed25519KeyHash1
        res `shouldEqual` Nothing
    test "getPubKeyHashDelegationsAndRewards #2" do
      noWallet do
        void $ getPubKeyHashDelegationsAndRewards $ StakePubKeyHash $ wrap
          ed25519KeyHash2

ed25519KeyHash2 :: Ed25519KeyHash
ed25519KeyHash2 = unsafePartial $ fromJust $ ed25519KeyHashFromBytes $
  hexToByteArrayUnsafe
    "541d6a23b07ebe1363671f49c833f6c33176ec968de1482fdf15cc1f"
