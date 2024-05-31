module Ctl.Internal.Testnet.Server
  ( checkTestnet
  , runTestnetTestPlan
  , startTestnet
  , stopTestnet
  , startCardanoTestnet
  , testTestnetContracts
  , startTestnetCluster
  ) where

import Prelude

import Contract.Prelude (Effect, either, liftEffect, liftEither, log, traverse)
import Contract.Test.Mote (TestPlanM)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Server (startOgmios)
import Ctl.Internal.Plutip.Spawn (ManagedProcess(..), _rmdirSync, spawn)
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , PlutipConfig
  , StopClusterResponse
  )
import Ctl.Internal.Plutip.Utils
  ( mkDirIfNotExists
  , onLine
  , tmpdir
  , waitForEvent
  )
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Test.ContractTest
  ( ContractTest
  , ContractTestPlan(ContractTestPlan)
  )
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Testnet.Utils
  ( findTestnetPaths
  , onTestnetEvent
  , toAbsolutePaths
  , waitFor
  )
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.Posix.Signal (Signal(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff, forkAff, try)
import Effect.Aff as Aff
import Effect.Exception (message)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , Event(..)
  , TestnetPaths
  )
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
startTestnetCluster
  :: CardanoTestnetStartupParams
  -> Ref (Array (Aff Unit))
  -> { ogmiosConfig :: ServerConfig
     , kupoConfig :: ServerConfig
     }
  -> Aff
       { ogmios :: ManagedProcess
       , testnet :: ManagedProcess
       , paths :: TestnetPaths
       }
startTestnetCluster startupParams cleanupRef cfg = do
  { process: process@(ManagedProcess _ testnetProcess _)
  , workdir
  } <- startCardanoTestnet startupParams cleanupRef
  source <- liftEffect
    $ onTestnetEvent
    $ Node.ChildProcess.stdout testnetProcess
  waitFor source case _ of
    Ready -> Just unit
    _ -> Nothing
  paths <- liftEffect
    $
      map (toAbsolutePaths { workdir })
        <<< liftEither
        =<< findTestnetPaths { workdir }
  ogmios <- after
    (startOgmios cfg paths)
    \(ManagedProcess _ ogmiosProcess _) ->
      liftEffect
        $ addCleanup cleanupRef
        $ liftEffect
        $ Node.ChildProcess.kill SIGINT ogmiosProcess
  redirectLogging
    { stderr:
        { logFile: Just $ workdir <</>> "ogmios-stderr.log"
        , handleLine: append "[ogmios][error]: " >>> log
        }
    , stdout:
        { logFile: Just $ workdir <</>> "ogmios-stdout.log"
        , handleLine: append "[ogmios]: " >>> log
        }
    }
    ogmios
  pure { paths, ogmios, testnet: process }

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
runTestnetTestPlan plutipCfg (ContractTestPlan runContractTestPlan) =
  unsafeThrow "runTestnetTestPlan"
    startCardanoTestnet

-- | Runs cardano-testnet executable with provided params.
spawnCardanoTestnet
  :: CardanoTestnetStartupParams
  -> { workdir :: FilePath }
  -> Aff ManagedProcess
spawnCardanoTestnet params { workdir } = do
  env <- Object.insert "TMPDIR" workdir <$> liftEffect Node.Process.getEnv
  spawn
    "cardano-testnet"
    options
    (defaultSpawnOptions { cwd = Just workdir, env = Just env })
    Nothing
  where
  flag :: String -> String
  flag name = "--" <> name

  option :: forall a. Show a => String -> a -> Array String
  option name value = [ flag name, show value ]

  moption :: forall a. Show a => String -> Maybe a -> Array String
  moption name value = option name =<< Array.fromFoldable value

  options :: Array String
  options = join
    [ [ "cardano" ]
    , option "testnet-magic" params.testnetMagic
    , Array.fromFoldable $ flag <<< show <$> params.era
    , moption "active-slots-coeff" params.activeSlotsCoeff
    , moption "enable-p2p" params.enableP2p
    , moption "nodeLoggingFormat" params.nodeLoggingFormat
    , moption "num-pool-nodes" params.numPoolNodes
    , moption "epoch-length" params.epochLength
    , moption "slot-length" params.slotLength
    ]

startCardanoTestnet
  :: CardanoTestnetStartupParams
  -> Ref (Array (Aff Unit))
  -> Aff
       { process :: ManagedProcess
       , workdir :: FilePath
       }
startCardanoTestnet params cleanupRef = do

  tmp <- liftEffect tmpdir
  randomStr <- liftEffect $ uniqueId ""
  let
    workdir = tmp <</>> "cardano-testnet-instance-" <> randomStr
  liftEffect $ mkDirIfNotExists workdir

  -- clean up on SIGINT
  shouldCleanup <- liftEffect $ Node.Process.lookupEnv "TESTNET_CLEANUP_WORKDIR"
    <#> case _ of
      Just "0" -> false
      _ -> true
  when shouldCleanup
    $ liftEffect
    $ addCleanup cleanupRef
    $ liftEffect do
        log "Cleaning up workidr"
        _rmdirSync workdir

  process <- spawnCardanoTestnet params { workdir }

  -- forward node's stdout
  redirectLogging
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

  pure { process, workdir }

redirectLogging
  :: { stderr ::
         { logFile :: Maybe FilePath
         , handleLine :: String -> Effect Unit
         }
     , stdout ::
         { logFile :: Maybe FilePath
         , handleLine :: String -> Effect Unit
         }
     }
  -> ManagedProcess
  -> Aff Unit
redirectLogging { stderr, stdout } (ManagedProcess _ child _) = do
  _ <- redirect stdout (Node.ChildProcess.stdout child)
  _ <- redirect stderr (Node.ChildProcess.stderr child)
  pure unit
  where
  logErrors = flip catchError
    (message >>> append "redirectLogging: callback error: " >>> log)
  redirect { handleLine, logFile } readable = do
    events <- liftEffect $ onLine readable Just
    _ <- forkAff $ flip tailRecM unit \_ -> do
      line <- waitForEvent events
      liftEffect $ logErrors $ void do
        handleLine line
        traverse (flip (appendTextFile UTF8) $ line <> "\n") logFile
      pure $ Loop unit
    pure unit

addCleanup :: Ref (Array (Aff Unit)) -> Aff Unit -> Effect Unit
addCleanup = map void <<< flip (Ref.modify <<< Array.cons <<< reportError)
  where
  reportError action = do
    try action >>= either
      (log <<< append "[addCleanup][error]: " <<< message)
      (const $ pure unit)

-- | Just as a bracket but without the body.
after :: forall a. Aff a -> (a -> Aff Unit) -> Aff a
after first second = Aff.bracket first second pure