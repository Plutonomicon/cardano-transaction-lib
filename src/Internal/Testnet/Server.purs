module Ctl.Internal.Testnet.Server
  ( checkTestnet
  , runTestnetTestPlan
  , startTestnet
  , stopTestnet
  , startCardanoTestnet
  , testTestnetContracts
  , startTestnetCluster
  , defaultStartupParams
  , StartedTestnetCluster(MkStartedTestnetCluster)
  , Channels
  ) where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM)
import Control.Apply (applySecond)
import Control.Monad.Error.Class
  ( throwError
  )
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Server
  ( startKupo
  , startOgmios
  )
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess(..)
  , _rmdirSync
  , killProcessWithPort
  , spawn
  , stop
  )
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , PlutipConfig
  , StopClusterResponse
  )
import Ctl.Internal.Plutip.Utils
  ( EventSource(..)
  , addCleanup
  , annotateError
  , narrowEventSource
  , onLine
  , runCleanup
  , scheduleCleanup
  , suppressAndLogErrors
  , tmpdir
  , tryAndLogErrors
  , waitForClose
  , waitForEvent
  )
import Ctl.Internal.Test.ContractTest
  ( ContractTest
  , ContractTestPlan(ContractTestPlan)
  )
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , KupmiosConfig
  , Node
  , TestnetPaths
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Testnet.Utils
  ( findNodeDirs
  , findTestnetPaths
  , onTestnetEvent
  , readNodes
  , tellsIt'sLocation
  , waitFor
  )
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
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

defaultStartupParams :: CardanoTestnetStartupParams
defaultStartupParams =
  ( Testnet.Types.defaultStartupParams
      { testnetMagic: 2 }
  )
    { nodeLoggingFormat = Just Testnet.Types.LogAsJson }

newtype StartedTestnetCluster = MkStartedTestnetCluster
  { ogmios ::
      { process :: ManagedProcess
      , channels :: Channels String
      }
  , kupo ::
      { process :: ManagedProcess
      , channels :: Channels String
      , workdir :: FilePath
      }
  , testnet ::
      { process :: ManagedProcess
      , channels :: Channels String
      }
  , paths :: TestnetPaths
  }

-- | Start the plutip cluster, initializing the state with the given
-- | UTxO distribution. Also initializes an extra payment key (aka
-- | `ourKey`) with some UTxOs for use with further plutip
-- | setup. `ourKey` has funds proportional to the total amount of the
-- | UTxOs in the passed distribution, so it can be used to handle
-- | transaction fees.
startTestnetCluster
  :: forall r
   . CardanoTestnetStartupParams
  -> Ref (Array (Aff Unit))
  -> Record (KupmiosConfig r)
  -> Aff StartedTestnetCluster
startTestnetCluster startupParams cleanupRef cfg = do
  { testnet
  , channels
  , workdirAbsolute
  } <- annotateError "Could not start cardano-testnet"
    $ startCardanoTestnet startupParams cleanupRef

  testnetEvents <- liftEffect $ onTestnetEvent channels.stdout
  -- it will crash right here uncatchable if testnet process will die
  log "Waiting for Ready state"
  waitFor testnetEvents case _ of
    Testnet.Types.Ready -> Just unit
    _ -> Nothing
  log "Testnet is ready"

  paths <- liftEither
    =<< liftEffect (findTestnetPaths { workdir: workdirAbsolute })

  ogmios <- annotateError "Could not start ogmios"
    $ startOgmios' { paths, workdir: workdirAbsolute }
  kupo <- annotateError "Could not start kupo"
    $ startKupo' { paths, workdir: workdirAbsolute }

  pure $ MkStartedTestnetCluster
    { paths
    , ogmios
    , kupo
    , testnet: { process: testnet, channels }
    }
  where
  startKupo' { paths, workdir } = do
    kupo /\ kupoWorkdir <-
      scheduleCleanup
        cleanupRef
        (startKupo cfg paths cleanupRef)
        $ fst
        >>> stop

    _ <- Aff.forkAff do
      waitForClose kupo
      runCleanup cleanupRef
      throwError $ error "kupo process has exited"

    kupoChannels <- liftEffect $ getChannels kupo
    _ <- redirectChannels
      kupoChannels
      { stderrTo:
          { log: workdir <</>> "kupo.stderr.log"
          , console: Just "[kupo][error]: "
          }
      , stdoutTo: { log: workdir <</>> "kupo.stdout.log", console: Nothing }
      }
    pure { process: kupo, workdir: kupoWorkdir, channels: kupoChannels }

  startOgmios' { paths, workdir } = do
    ogmios <- scheduleCleanup
      cleanupRef
      (startOgmios cfg paths)
      stop
    _ <- Aff.forkAff do
      waitForClose ogmios
      runCleanup cleanupRef
      throwError $ error "ogmios process has exited"

    ogmiosChannels <- liftEffect $ getChannels ogmios
    _ <- redirectChannels
      ogmiosChannels
      { stderrTo:
          { log: workdir <</>> "ogmios.stderr.log"
          , console: Just "[ogmios][error]: "
          }
      , stdoutTo: { log: workdir <</>> "ogmios.stdout.log", console: Nothing }
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
  -> Aff ManagedProcess
spawnCardanoTestnet params = do
  spawn
    "cardano-testnet"
    options
    defaultSpawnOptions
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
       , workdirAbsolute :: FilePath
       , nodes :: Array { | Node () }
       }
startCardanoTestnet params cleanupRef = do

  tmp <- liftEffect tmpdir

  testnet <- spawnCardanoTestnet params
  channels <- liftEffect $ getChannels testnet

  _ <- Aff.forkAff do
    waitForClose testnet
    runCleanup cleanupRef
    throwError $ error "cardano-testnet process has exited"

  workdirAbsolute <- do
    events@(EventSource { cancel }) <- liftEffect
      $ narrowEventSource
          (tellsIt'sLocation { tmpdir: tmp })
          channels.stdout
    workdir <- waitForEvent events
    log $ "Found workdir: " <> workdir
    liftEffect $ cancel $ error "Not needed anymore"
    pure $ tmp <</>> workdir

  -- cardano-testnet doesn't kill cardano-nodes it spawns, so we do it ourselves
  nodes <- liftEffect do
    nodeDirs <- findNodeDirs { workdir: workdirAbsolute }
    readNodes { testnetDirectory: workdirAbsolute, nodeDirs }

  for_ nodes \{ port } -> do
    liftEffect
      $ addCleanup cleanupRef
      $ killProcessWithPort port

  -- clean up on SIGINT
  do
    shouldCleanup <- liftEffect
      $ Node.Process.lookupEnv "TESTNET_CLEANUP_WORKDIR"
      <#> case _ of
        Just "0" -> false
        _ -> true
    when shouldCleanup
      $ liftEffect
      $ addCleanup cleanupRef
      $ liftEffect do
          log "Cleaning up workidr"
          _rmdirSync workdirAbsolute

  -- forward node's stdout
  _ <- redirectChannels
    { stderr: channels.stderr, stdout: channels.stdout }
    { stdoutTo:
        { log: workdirAbsolute <</>> "cardano-testnet.stdout.log"
        , console: Just "[cardano-testnet][info]"
        }
    , stderrTo:
        { log: workdirAbsolute <</>> "cardano-testnet.stderr.log"
        , console: Just "[cardano-testnet][error]"
        }
    }

  pure { testnet, workdirAbsolute, channels, nodes }

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

-- Note: it will not throw, so to check the computation result
-- Fiber must be inspected.
redirectChannels
  :: { stderr :: EventSource String
     , stdout :: EventSource String
     }
  -> { stderrTo :: { log :: FilePath, console :: Maybe String }
     , stdoutTo :: { log :: FilePath, console :: Maybe String }
     }
  -> Aff (Aff.Fiber (Either Error Unit))
redirectChannels { stderr, stdout } { stderrTo, stdoutTo } = do
  handleStderr <- redirectLogging
    stderr
    { storeLogs: Just
        { logFile: stderrTo.log
        , toString: identity
        }
    , handleLine: case stderrTo.console of
        Nothing -> const $ pure unit
        Just prefix -> append prefix >>> log
    }
  handleStdout <- redirectLogging
    stdout
    { storeLogs: Just
        { logFile: stdoutTo.log
        , toString: identity
        }
    , handleLine: case stdoutTo.console of
        Nothing -> const $ pure unit
        Just prefix -> append prefix >>> log
    }
  pure $ applySecond <$> handleStderr <*> handleStdout

redirectLogging
  :: forall a
   . EventSource a
  -> { storeLogs ::
         Maybe
           { logFile :: FilePath
           , toString :: a -> String
           }
     , handleLine :: a -> Effect Unit
     }
  -> Aff (Aff.Fiber (Either Error Unit))
redirectLogging events { handleLine, storeLogs } =
  Aff.forkAff $ tryAndLogErrors "redirectLogging" $ flip tailRecM unit \_ -> do
    line <- waitForEvent events
    liftEffect $ suppressAndLogErrors "redirectLogging: callback error" $ void
      do
        handleLine line
        for storeLogs \{ logFile, toString } ->
          appendTextFile UTF8 logFile $ toString line <> "\n"
    pure $ Loop unit
