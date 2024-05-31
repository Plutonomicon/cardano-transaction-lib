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
  ( EventSource
  , mkDirIfNotExists
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

type Channels a =
  { stderr :: EventSource a
  , stdout :: EventSource a
  }

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
       { ogmios ::
           { process :: ManagedProcess
           , channels :: Channels String
           }
       , testnet ::
           { process :: ManagedProcess
           , channels :: Channels String
           }
       , paths :: TestnetPaths
       }
startTestnetCluster startupParams cleanupRef cfg = do
  { testnet
  , channels
  , workdir
  } <- startCardanoTestnet startupParams cleanupRef
  source <- liftEffect $ onTestnetEvent channels.stdout

  -- it will crash right here if testnet process will die
  waitFor source case _ of
    Ready -> Just unit
    _ -> Nothing

  paths <-
    map (toAbsolutePaths { workdir })
      <<< liftEither
      =<< liftEffect (findTestnetPaths { workdir })

  ogmios <- startOgmios' { paths, workdir }

  pure
    { paths
    , ogmios: ogmios
    , testnet: { process: testnet, channels }
    }
  where
  startOgmios' { paths, workdir } = do
    ogmios <- after
      (startOgmios cfg paths)
      \(ManagedProcess _ ogmiosProcess _) ->
        liftEffect
          $ addCleanup cleanupRef
          $ liftEffect
          $ Node.ChildProcess.kill SIGINT ogmiosProcess

    ogmiosChannels <- liftEffect $ getChannels ogmios
    redirectLogging
      ogmiosChannels.stderr
      { logFile: Just $ workdir <</>> "ogmios-stderr.log"
      , handleLine: append "[ogmios][error]: " >>> log
      }
    redirectLogging
      ogmiosChannels.stdout
      { logFile: Just $ workdir <</>> "ogmios-stdout.log"
      , handleLine: append "[ogmios]: " >>> log
      }
    pure { process: ogmios, channels: ogmiosChannels }

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
       { testnet :: ManagedProcess
       , channels ::
           { stderr :: EventSource String
           , stdout :: EventSource String
           }
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

  testnet <- spawnCardanoTestnet params { workdir }
  channels <- liftEffect $ getChannels testnet

  -- forward node's stdout
  redirectLogging channels.stdout
    { logFile: Just $ workdir <</>> "cardano-testnet-stdout.log"
    , handleLine: log <<< append "[cardano-node-stdout]"
    }
  redirectLogging channels.stderr
    { logFile: Just $ workdir <</>> "cardano-testnet-stderr.log"
    , handleLine: log <<< append "[cardano-node-stderr]"
    }

  pure { testnet, workdir, channels }

getChannels
  :: ManagedProcess
  -> Effect
       { stderr :: EventSource String
       , stdout :: EventSource String
       }
getChannels (ManagedProcess _ process _) = ado
  stdout <- onLine (Node.ChildProcess.stdout process) Just
  stderr <- onLine (Node.ChildProcess.stderr process) Just
  in { stdout, stderr }

redirectLogging
  :: forall a
   . Show a
  => EventSource a
  -> { logFile :: Maybe FilePath
     , handleLine :: a -> Effect Unit
     }
  -> Aff Unit
redirectLogging events { handleLine, logFile } =
  void $ forkAff $ flip tailRecM unit \_ -> do
    line <- waitForEvent events
    liftEffect $ logErrors $ void do
      handleLine line
      traverse (flip (appendTextFile UTF8) $ show line <> "\n") logFile
    pure $ Loop unit
  where
  logErrors = flip catchError
    $ message >>> append "redirectLogging: callback error: " >>> log

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