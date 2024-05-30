module Ctl.Internal.Testnet.Server
  ( checkTestnet
  , runTestnetTestPlan
  , startTestnet
  , stopTestnet
  , startCardanoTestnet
  , testTestnetContracts
  )
  where

import Prelude

import Contract.Prelude (liftEffect)
import Contract.Test.Mote (TestPlanM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Spawn (ManagedProcess(..), _rmdirSync, spawn, waitForSignal)
import Ctl.Internal.Plutip.Types (ClusterStartupParameters, PlutipConfig, StopClusterResponse)
import Ctl.Internal.Plutip.Utils (mkDirIfNotExists, tmpdir)
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Ctl.Internal.Test.ContractTest (ContractTest, ContractTestPlan(ContractTestPlan))
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Posix.Signal (Signal(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff, forkAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Internal.Testnet.Types (CardanoTestnetStartupParams)
import Node.ChildProcess (defaultSpawnOptions)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (appendTextFile)
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.ReadLine as RL

-- | Run several `Contract`s in tests in a (single) Testnet environment (cardano-testnet, kupo, etc.).
-- | NOTE: This uses `MoteT`s bracketing, and thus has the same caveats.
-- |       Namely, brackets are run for each of the top-level groups and tests
-- |       inside the bracket.
-- |       If you wish to only set up Testnet once, ensure all tests that are passed
-- |       to `testTestnetContracts` are wrapped in a single group.
testTestnetContracts
  :: PlutipConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
testTestnetContracts testnetCfg tp = unsafeThrow "sdfsd"

checkTestnet :: PlutipConfig -> Aff Unit
checkTestnet cfg = unsafeThrow "checkTestnet"

-- | Start the plutip cluster, initializing the state with the given
-- | UTxO distribution. Also initializes an extra payment key (aka
-- | `ourKey`) with some UTxOs for use with further plutip
-- | setup. `ourKey` has funds proportional to the total amount of the
-- | UTxOs in the passed distribution, so it can be used to handle
-- | transaction fees.
startTestnet
  :: PlutipConfig
  -> InitialUTxODistribution
  -> Aff (ManagedProcess /\ PrivatePaymentKey /\ ClusterStartupParameters)
startTestnet _ = unsafeThrow "startTestnet"

stopTestnet :: PlutipConfig -> Aff StopClusterResponse
stopTestnet cfg = unsafeThrow "stopTestnet"

-- | Run a `ContractTestPlan` in a (single) Testnet environment.
-- | Supports wallet reuse - see docs on sharing wallet state between
-- | wallets in `doc/plutip-testing.md`.
runTestnetTestPlan
  :: PlutipConfig
  -> ContractTestPlan
  -> TestPlanM (Aff Unit) Unit
runTestnetTestPlan plutipCfg (ContractTestPlan runContractTestPlan) = unsafeThrow "runTestnetTestPlan"

-- | Runs cardano-testnet executable with provided params.
spawnCardanoTestnet ::
  CardanoTestnetStartupParams
  -> { workdir :: FilePath }
  -> Aff ManagedProcess
spawnCardanoTestnet params {workdir} = do
  spawn
    "cardano-testnet"
    options
    (defaultSpawnOptions {cwd = Just workdir})
    Nothing
  where
    flag :: String -> String
    flag name = "--" <> name
    option :: forall a. Show a => String -> a -> String
    option name value = flag name <> " " <> show value
    options :: Array String
    options = catMaybes
      [ Just $ option "testnet-magic" params.testnetMagic
      , flag <<< show <$> params.era
      , option "active-slots-coeff" <$> params.activeSlotsCoeff
      , option "enable-p2p" <$> params.enableP2p
      , option "node-logging-format" <$> params.nodeLoggingFormat
      , option "num-pool-nodes" <$> params.numPoolNodes
      , option "epoch-length" <$> params.epochLength
      , option "slot-length" <$> params.slotLength
      ]

startCardanoTestnet
  :: CardanoTestnetStartupParams
  -> Aff { process :: ManagedProcess, workdir :: FilePath }
startCardanoTestnet params = do
  tmp <- liftEffect tmpdir
  randomStr <- liftEffect $ uniqueId ""
  let
    workdir = tmp <</>> randomStr <> "-cardano-testnet-instance" 
  liftEffect $ mkDirIfNotExists workdir
  -- clean up on SIGINT
  _ <- forkAff do
    waitForSignal SIGINT
    liftEffect $ _rmdirSync workdir

  process@(ManagedProcess _ child _) <- spawnCardanoTestnet params {workdir} 

  -- forward node's stdout
  do
    interface <- liftEffect $ RL.createInterface (Node.ChildProcess.stdout child) mempty
    liftEffect $ flip RL.setLineHandler interface
      \str -> appendTextFile UTF8 (workdir <</>> "log") $ "[cardano-testnet]: " <> str

  pure {process, workdir}
