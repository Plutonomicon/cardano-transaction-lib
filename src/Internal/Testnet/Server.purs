module Ctl.Internal.Testnet.Server
  ( checkTestnet
  , runTestnetTestPlan
  , startTestnet
  , stopChildProcessWithPort
  , stopTestnet
  , testTestnetContracts
  )
  where

import Prelude

import Contract.Test.Mote (TestPlanM)
import Ctl.Internal.Plutip.Spawn (ManagedProcess, spawn)
import Ctl.Internal.Plutip.Types (ClusterStartupParameters, PlutipConfig, StopClusterResponse)
import Ctl.Internal.Test.ContractTest (ContractTest, ContractTestPlan(ContractTestPlan))
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import Internal.Testnet.Types (CardanoTestnetStartupParams)
import Node.ChildProcess (defaultSpawnOptions)

-- | Run several `Contract`s in tests in a (single) Testnet environment (cardano-testnet, kupo, etc.).
-- | NOTE: This uses `MoteT`s bracketing, and thus has the same caveats.
-- |       Namely, brackets are run for each of the top-level groups and tests
-- |       inside the bracket.
-- |       If you wish to only set up Testnet once, ensure all tests that are passed
-- |       to `testTestnetContracts` are wrapped in a single group.
testTestnetContracts
  :: TestnetConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
testTestnetContracts testnetCfg tp = do