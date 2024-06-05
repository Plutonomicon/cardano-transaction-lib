module Ctl.Internal.Testnet.Server
  ( checkTestnet
  , runTestnetTestPlan
  , redirectChannels
  , startTestnet
  , stopTestnet
  , startCardanoTestnet
  , testTestnetContracts
  , startTestnetCluster
  , StartedTestnetCluster(MkStartedTestnetCluster)
  , Channels
  ) where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM)
import Control.Alt ((<|>))
import Control.Apply (applySecond)
import Control.Monad.Error.Class (liftMaybe, throwError, try)
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
  ( EventSource
  , addCleanup
  , annotateError
  , mkDirIfNotExists
  , onLine
  , runCleanup
  , scheduleCleanup
  , suppressAndLogErrors
  , tmpdir
  , tryAndLogErrors
  , waitForClose
  , waitForError
  , waitForEvent
  , waitUntil
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
  , findTestnetWorkir
  , getRuntime
  , readNodes
  )
import Ctl.Internal.Wallet.Key (PrivatePaymentKey)
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Random (randomInt)
import Effect.Ref (Ref)
import Foreign.Object as Object
import Node.ChildProcess (defaultSpawnOptions)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as Node.FS
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
   . Record (CardanoTestnetStartupParams (KupmiosConfig r))
  -> Ref (Array (Aff Unit))
  -> Aff StartedTestnetCluster
startTestnetCluster startupParams cleanupRef = do
  { testnet
  , channels
  , workdirAbsolute
  } <- annotateError "Could not start cardano-testnet"
    $ startCardanoTestnet startupParams cleanupRef

  -- testnetEvents <- liftEffect $ onTestnetEvent channels.stdout
  -- it will crash right here uncatchable if testnet process will die
  log "Waiting for Ready state"
  -- waitFor testnetEvents case _ of
  --   Testnet.Types.Ready -> Just unit
  --   _ -> Nothing

  { runtime, paths } <- waitUntil (Milliseconds 4000.0)
    $ map hush
    $ tryAndLogErrors "Waiting for ready state"
    $ liftEffect do

        paths <- liftEither =<< findTestnetPaths { workdir: workdirAbsolute }
        runtime <- getRuntime paths
        pure { runtime, paths }

  log "Testnet is ready"
  Aff.delay $ Milliseconds 2000.0

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
        (startKupo startupParams paths cleanupRef)
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
          { log: Just $ workdir <</>> "kupo.stderr.log"
          , console: Just "[kupo][error]: "
          }
      , stdoutTo:
          { log: Just $ workdir <</>> "kupo.stdout.log", console: Nothing }
      }
    pure { process: kupo, workdir: kupoWorkdir, channels: kupoChannels }

  startOgmios' { paths, workdir } = do
    ogmios <- scheduleCleanup
      cleanupRef
      (startOgmios startupParams paths)
      stop
    _ <- Aff.forkAff do
      waitForClose ogmios
      runCleanup cleanupRef
      throwError $ error "ogmios process has exited"

    ogmiosChannels <- liftEffect $ getChannels ogmios
    _ <- redirectChannels
      ogmiosChannels
      { stderrTo:
          { log: Just $ workdir <</>> "ogmios.stderr.log"
          , console: Just "[ogmios][error]: "
          }
      , stdoutTo:
          { log: Just $ workdir <</>> "ogmios.stdout.log", console: Nothing }
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
  :: forall r
   . { cwd :: FilePath }
  -> { | CardanoTestnetStartupParams r }
  -> Aff ManagedProcess
spawnCardanoTestnet { cwd } params = do
  env <- liftEffect Node.Process.getEnv
  initCwd <- liftMaybe (error "Couldn't find INIT_CWD env variable")
    $ Object.lookup "INIT_CWD" env
  let
    env' = Object.fromFoldable
      [ "TMPDIR" /\ cwd -- only for 8.1.1; 8.7.2 puts it's testnet directory into cwd instead
      , "CARDANO_NODE_SRC" /\ (initCwd <</>> "cardano-testnet-files")
      , "CARDANO_CLI" /\ "cardano-cli"
      , "CREATE_SCRIPT_CONTEXT" /\ "create-script-context"
      , "CARDANO_NODE" /\ "cardano-node"
      , "CARDANO_SUBMIT_API" /\ "cardano-submit-api"
      , "CARDANO_NODE_CHAIRMAN" /\ "cardano-node-chairman"
      ]
    opts = defaultSpawnOptions
      { cwd = Just cwd, env = Just $ Object.union env' env }
  -- log $ show env
  spawn
    "cardano-testnet"
    options
    opts
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
  :: forall r
   . { | CardanoTestnetStartupParams r }
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
startCardanoTestnet params cleanupRef = annotateError "startCardanoTestnet" do

  testDir <- liftEffect do
    tmp <- tmpdir
    log $ show { tmp }
    testId <- randomInt 0 999
    let dir = tmp <</>> "testnet-" <> show testId
    mkDirIfNotExists dir
    log $ show { dir }
    pure dir

  let
    tmpLogDir = testDir <</>> "logs"
    tmpStdoutLogs = tmpLogDir <</>> "cardano-testnet.stdout.tmp.log"
    tmpStderrLogs = tmpLogDir <</>> "cardano-testnet.stderr.tmp.log"
    cleanupTmpLogs = do
      void $ try $ Node.FS.rm tmpStdoutLogs
      void $ try $ Node.FS.rm tmpStderrLogs

  testnet <- spawnCardanoTestnet { cwd: testDir } params
  channels <- liftEffect $ getChannels testnet

  -- Additional logging channels
  _ <- Aff.forkAff $ annotateError "startCardanoTestnet:waitForFail" do
    let
      waitError = Just <$> waitForError testnet
      waitClose = Nothing <$ waitForClose testnet
    cause <- waitError <|> waitClose
    runCleanup cleanupRef
    throwError $ fromMaybe (error "cardano-testnet process has exited") cause

  liftEffect $ mkDirIfNotExists tmpLogDir
  tempOutput <- redirectChannels
    { stderr: channels.stderr, stdout: channels.stdout }
    { stdoutTo: { log: Just tmpStdoutLogs, console: Just "[node][stdout]" }
    , stderrTo: { log: Just tmpStderrLogs, console: Just "[node][stderr]" }
    }
  log "Redirected logs"

  -- -- It may not create an own directory until show any signs of life 
  -- _ <- waitForEvent channels.stdout

  -- forward node's stdout
  workdirAbsolute <- waitUntil (Milliseconds 1000.0)
    $ liftEffect
    $ map hush
    $ try
    $ liftMaybe (error "First testnet dir is missing")
      =<< findTestnetWorkir { tmpdir: testDir, dirIdx: 0 }
  Aff.killFiber (error "Temp output is not needed anymore") tempOutput

  -- cardano-testnet doesn't kill cardano-nodes it spawns, so we do it ourselves

  nodes <- waitUntil (Milliseconds 3000.0)
    $ liftEffect
    $ map hush
    $ tryAndLogErrors "waiting until nodes are there" do

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
          cleanupTmpLogs
          _rmdirSync workdirAbsolute

  _ <- redirectChannels
    { stderr: channels.stderr, stdout: channels.stdout }
    { stdoutTo:
        { log: Just $ workdirAbsolute <</>> "cardano-testnet.stdout.log"
        , console: Nothing
        }
    , stderrTo:
        { log: Just $ workdirAbsolute <</>> "cardano-testnet.stderr.log"
        , console: Nothing
        }
    }
  log "startCardanoTestnet:done"
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
  -> { stderrTo :: { log :: Maybe FilePath, console :: Maybe String }
     , stdoutTo :: { log :: Maybe FilePath, console :: Maybe String }
     }
  -> Aff (Aff.Fiber (Either Error Unit))
redirectChannels { stderr, stdout } { stderrTo, stdoutTo } = do
  handleStderr <- redirectLogging
    stderr
    { storeLogs: stderrTo.log <#>
        { logFile: _
        , toString: identity
        }
    , handleLine: case stderrTo.console of
        Nothing -> const $ pure unit
        Just prefix -> append prefix >>> log
    }
  handleStdout <- redirectLogging
    stdout
    { storeLogs: stdoutTo.log <#>
        { logFile: _
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
          Node.FS.appendTextFile UTF8 logFile $ toString line <> "\n"
    pure $ Loop unit
