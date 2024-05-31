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

import Contract.Prelude (Effect, either, liftEffect, log, traverse)
import Contract.Test.Mote (TestPlanM)
import Control.Monad.Error.Class (catchError)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Spawn (ManagedProcess(..), _rmdirSync, spawn)
import Ctl.Internal.Plutip.Types (ClusterStartupParameters, PlutipConfig, StopClusterResponse)
import Ctl.Internal.Plutip.Utils (mkDirIfNotExists, tmpdir, waitForLine)
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Ctl.Internal.Test.ContractTest (ContractTest, ContractTestPlan(ContractTestPlan))
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Internal.Testnet.Types (CardanoTestnetStartupParams)
import Node.ChildProcess (defaultSpawnOptions)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (appendTextFile)
import Node.Path (FilePath)
import Node.Process as Node.Process

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
  env <- Object.insert "TMPDIR" workdir <$> liftEffect Node.Process.getEnv
  spawn
    "cardano-testnet"
    options
    (defaultSpawnOptions {cwd = Just workdir, env = Just env })
    Nothing
  where
    flag :: String -> String
    flag name = "--" <> name
    option :: forall a. Show a => String -> a -> Array String
    option name value = [flag name, show value]
    moption  :: forall a. Show a => String -> Maybe a -> Array String
    moption name value = option name =<< Array.fromFoldable value
    options :: Array String
    options = join
      [ ["cardano"]
      , option "testnet-magic" params.testnetMagic
      , Array.fromFoldable $ flag <<< show <$> params.era
      , moption "active-slots-coeff" params.activeSlotsCoeff
      , moption "enable-p2p" params.enableP2p
      , moption "node-logging-format" params.nodeLoggingFormat
      , moption "num-pool-nodes" params.numPoolNodes
      , moption "epoch-length" params.epochLength
      , moption "slot-length" params.slotLength
      ]

startCardanoTestnet
  :: CardanoTestnetStartupParams
  -> Ref (Array (Aff Unit))
  -> Aff { process :: ManagedProcess, workdir :: FilePath }
startCardanoTestnet params cleanupRef = do
  
  tmp <- liftEffect tmpdir
  randomStr <- liftEffect $ uniqueId ""
  let
    workdir = tmp <</>> "cardano-testnet-instance-" <> randomStr
  liftEffect $ mkDirIfNotExists workdir

  -- clean up on SIGINT
  shouldCleanup <- liftEffect $ Node.Process.lookupEnv "TESTNET_CLEANUP_WORKDIR" <#> case _ of
    Just "0" -> false
    _ -> true
  when shouldCleanup
    $ liftEffect
    $ addCleanup cleanupRef 
    $ liftEffect do
      log "Cleaning up workidr"
      _rmdirSync workdir

  process <- spawnCardanoTestnet params {workdir} 

  -- forward node's stdout
  liftEffect $ redirectLogging
    { stdout:
      { logFile: Just $ workdir <</>> "cardano-testnet-stdout.log"
      , handleLine: log <<< append "[cardano-node-stdout]"
      }
    , stderr:
      { logFile: Just $ workdir <</>> "cardano-testnet-stderr.log"
      , handleLine: log <<< append "[cardano-node-stderr]"
      }
    }
    process

  pure {process, workdir}

redirectLogging ::
  { stderr ::
    { logFile :: Maybe FilePath
    , handleLine :: String -> Effect Unit
    }
  , stdout ::
    { logFile :: Maybe FilePath
    , handleLine :: String -> Effect Unit
    }
  } ->
  ManagedProcess ->
  Effect Unit
redirectLogging {stderr,stdout} (ManagedProcess _ child _) = do
    _ <- redirect stdout (Node.ChildProcess.stdout child)
    _ <- redirect stderr (Node.ChildProcess.stderr child)
    pure unit
  where
    redirect {handleLine, logFile} readable = waitForLine readable \str ->
      flip catchError (message >>> append "redirectLogging: callback error: " >>> log)
        $ void do
          handleLine str
          traverse (flip (appendTextFile UTF8) $ str <> "\n") logFile

addCleanup :: Ref (Array (Aff Unit)) -> Aff Unit -> Effect Unit
addCleanup = map void <<< flip (Ref.modify <<< Array.cons <<< reportError)
  where
    reportError action = do
      try action >>= either
        (log <<< append "[addCleanup][error]: " <<< message)
        (const $ pure unit)