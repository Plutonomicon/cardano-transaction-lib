module Ctl.Internal.Testnet.Server
  ( Channels
  , StartedTestnetCluster(MkStartedTestnetCluster)
  , startKupo
  , startOgmios
  , startTestnetCluster
  , makeClusterContractEnv
  , mkLogging
  ) where

import Contract.Prelude hiding (log)

import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types.BigNum (maxValue, toString) as BigNum
import Contract.Config (Hooks, defaultSynchronizationParams, defaultTimeParams)
import Contract.Monad (ContractEnv)
import Control.Alt ((<|>))
import Control.Apply (applySecond)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(Loop), tailRecM)
import Ctl.Internal.Contract.Monad
  ( buildBackend
  , getLedgerConstants
  , mkQueryHandle
  , stopContractEnv
  )
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Helpers (concatPaths, (<</>>))
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Spawn
  ( ManagedProcess(ManagedProcess)
  , NewOutputAction(NoOp, Success)
  , _rmdirSync
  , isPortAvailable
  , spawn
  , stop
  , stopProcessWithChildren
  )
import Ctl.Internal.Testnet.Types
  ( TestnetClusterConfig
  , TestnetConfig
  , TestnetPaths
  )
import Ctl.Internal.Testnet.Utils
  ( EventSource
  , TestnetCleanupRef
  , addCleanup
  , after
  , annotateError
  , findTestnetPaths
  , getRuntime
  , onLine
  , runCleanup
  , scheduleCleanup
  , suppressAndLogErrors
  , tmpdirUnique
  , tryAndLogErrors
  , waitForClose
  , waitForError
  , waitForEvent
  , waitUntil
  )
import Ctl.Internal.Types.UsedTxOuts (newUsedTxOuts)
import Data.Array (head) as Array
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Set as Set
import Data.String (split, stripPrefix, trim) as String
import Data.String.CodeUnits (indexOf) as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UInt (UInt)
import Data.UInt (toString) as UInt
import Effect.AVar (tryPut) as AVarSync
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar (empty, take) as AVar
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, throw)
import Effect.Ref (Ref)
import Effect.Ref (modify_, new) as Ref
import Foreign.Object as Object
import Node.ChildProcess (defaultSpawnOptions, stdout)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readdir) as FSSync
import Node.FS.Sync as Node.FS
import Node.Path (FilePath)
import Node.Process as Node.Process
import Node.Stream (onDataString)

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

derive instance Newtype StartedTestnetCluster _

startOgmios
  :: forall r r'
   . { ogmiosConfig :: ServerConfig | r }
  -> { nodeSocketPath :: FilePath
     , nodeConfigPath :: FilePath
     | r'
     }
  -> Aff ManagedProcess
startOgmios cfg params = do
  spawn "ogmios" ogmiosArgs defaultSpawnOptions
    $ Just
    $ _.output
    >>> String.indexOf (Pattern "networkParameters")
    >>> maybe NoOp (const Success)
    >>> pure
  where
  ogmiosArgs :: Array String
  ogmiosArgs =
    [ "--host"
    , cfg.ogmiosConfig.host
    , "--port"
    , UInt.toString cfg.ogmiosConfig.port
    , "--node-socket"
    , params.nodeSocketPath
    , "--node-config"
    , params.nodeConfigPath
    , "--include-transaction-cbor"
    ]

startKupo
  :: forall r r'
   . { kupoConfig :: ServerConfig | r }
  -> { nodeSocketPath :: FilePath
     , nodeConfigPath :: FilePath
     | r'
     }
  -> Ref (Array (Aff Unit))
  -> Aff (ManagedProcess /\ String)
startKupo cfg params cleanupRef = do
  workdir <- tmpdirUnique "kupo-db"
  childProcess <-
    after
      (spawnKupoProcess workdir)
      -- set up cleanup
      $ const
      $ liftEffect
      $ addCleanup cleanupRef
      $ liftEffect
      $ _rmdirSync workdir
  pure (childProcess /\ workdir)
  where
  spawnKupoProcess :: FilePath -> Aff ManagedProcess
  spawnKupoProcess workdir =
    spawn "kupo" (kupoArgs workdir) defaultSpawnOptions $
      Just
        ( _.output >>> String.indexOf outputString
            >>> maybe NoOp (const Success)
            >>> pure
        )
    where
    outputString :: Pattern
    outputString = Pattern "ConfigurationCheckpointsForIntersection"

  kupoArgs :: FilePath -> Array String
  kupoArgs workdir =
    [ "--match"
    , "*/*"
    , "--since"
    , "origin"
    , "--workdir"
    , workdir
    , "--host"
    , cfg.kupoConfig.host
    , "--port"
    , UInt.toString cfg.kupoConfig.port
    , "--node-socket"
    , params.nodeSocketPath
    , "--node-config"
    , params.nodeConfigPath
    ]

-- | Start the testnet cluster, initializing the state with the given
-- | UTxO distribution. Also initializes an extra payment key (aka
-- | `ourKey`) with some UTxOs for use with further testnet
-- | setup. `ourKey` has funds proportional to the total amount of the
-- | UTxOs in the passed distribution, so it can be used to handle
-- | transaction fees.
startTestnetCluster
  :: TestnetConfig
  -> Ref (Array (Aff Unit))
  -> Logger
  -> Aff StartedTestnetCluster
startTestnetCluster cfg cleanupRef logger = do
  { testnet, channels, workdirAbsolute } <-
    annotateError "Could not start cardano-testnet" $
      startCardanoTestnet cfg.clusterConfig cleanupRef logger

  { paths } <- waitUntil (Milliseconds 4000.0)
    $ map hush
    $ tryAndLogErrors "Waiting for ready state"
    $ liftEffect do

        paths <- liftEither =<< findTestnetPaths { workdir: workdirAbsolute }
        runtime <- getRuntime paths
        pure { runtime, paths }

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
        (stopChildProcessWithPort cfg.kupoConfig.port <<< fst)

    void $ Aff.forkAff (waitForClose kupo *> runCleanup cleanupRef)

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
    ogmios <-
      scheduleCleanup
        cleanupRef
        (startOgmios cfg paths)
        (stopChildProcessWithPort cfg.ogmiosConfig.port)

    void $ Aff.forkAff (waitForClose ogmios *> runCleanup cleanupRef)

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

-- | Spawns cardano-testnet process with provided parameters.
spawnCardanoTestnet :: FilePath -> TestnetClusterConfig -> Aff ManagedProcess
spawnCardanoTestnet workdir params = do
  env <- liftEffect Node.Process.getEnv
  let
    env' = Object.fromFoldable
      [ "CARDANO_CLI" /\ "cardano-cli"
      , "CARDANO_NODE" /\ "cardano-node"
      , "TMPDIR" /\ workdir
      ]
    opts = defaultSpawnOptions
      { cwd = Just workdir
      , env = Just $ Object.union env' env
      , detached = true
      }
  spawn "cardano-testnet" options opts Nothing
  where
  flag :: String -> String
  flag name = "--" <> name

  options :: Array String
  options = join
    [ [ "cardano" ]
    , [ flag $ show params.era ]
    , maybe mempty
        (\epochSize -> [ flag "epoch-length", UInt.toString epochSize ])
        params.epochSize
    , [ flag "slot-length", show (unwrap params.slotLength) ]
    , [ flag "testnet-magic", show params.testnetMagic ]
    , [ flag "max-lovelace-supply", BigNum.toString BigNum.maxValue ]
    ]

startCardanoTestnet
  :: TestnetClusterConfig
  -> TestnetCleanupRef
  -> Logger
  -> Aff
       { testnet :: ManagedProcess
       , channels ::
           { stderr :: EventSource String
           , stdout :: EventSource String
           }
       , workdirAbsolute :: FilePath
       }
startCardanoTestnet params cleanupRef logger =
  annotateError "startCardanoTestnet" do
    workdir <- tmpdirUnique "cardano-testnet"
    testnet@(ManagedProcess _ testnetProcess _) <- scheduleCleanup
      cleanupRef
      (spawnCardanoTestnet workdir params)
      stopProcessWithChildren

    workspaceFromLogsAvar <- AVar.empty
    liftEffect $ onDataString (stdout testnetProcess) UTF8 \str -> do
      let lines = String.split (Pattern "\n") str
      traverse_
        ( \line -> do
            logger Trace $ "[cardano-testnet:stdout] " <> line
            let
              mWorkspace = String.stripPrefix (Pattern "Workspace: ") $
                String.trim line
            maybe (pure unit)
              (void <<< flip AVarSync.tryPut workspaceFromLogsAvar)
              mWorkspace
        )
        lines

    workspace <- waitUntil (Milliseconds 100.0) $ findWorkspaceDir workdir
    -- Schedule a cleanup immediately after the workspace
    -- directory is created.
    scheduleWorkspaceCleanup workspace
    -- Wait for cardano-testnet to output the workspace, indicating
    -- that initialization is complete.
    workspaceFromLogs <- AVar.take workspaceFromLogsAvar

    when (workspace /= workspaceFromLogs) do
      runCleanup cleanupRef
      -- this error should never happen
      throwError $ error "cardano-testnet workspace mismatch"

    channels <- liftEffect $ getChannels testnet
    attachStdoutMonitors testnet
    pure { testnet, workdirAbsolute: workspace, channels }
  where
  findWorkspaceDir :: forall m. MonadEffect m => FilePath -> m (Maybe FilePath)
  findWorkspaceDir workdir =
    liftEffect $ map (concatPaths workdir) <<< Array.head <$>
      FSSync.readdir workdir

  attachStdoutMonitors :: ManagedProcess -> Aff Unit
  attachStdoutMonitors testnet =
    void $ Aff.forkAff $
      annotateError "startCardanoTestnet:attachStdoutMonitors" do
        let
          waitError = Just <$> waitForError testnet
          waitClose = Nothing <$ waitForClose testnet
        cause <- waitError <|> waitClose
        runCleanup cleanupRef
        throwError $ fromMaybe (error "cardano-testnet process has exited")
          cause

  scheduleWorkspaceCleanup :: forall m. MonadEffect m => FilePath -> m Unit
  scheduleWorkspaceCleanup workspace =
    liftEffect do
      shouldCleanup <-
        Node.Process.lookupEnv "TESTNET_CLEANUP_WORKDIR" <#>
          case _ of
            Just "0" -> false
            _ -> true
      when shouldCleanup do
        addCleanup cleanupRef $ liftEffect do
          logger Trace $ "Cleaning up cardano-testnet workspace: " <> workspace
          _rmdirSync workspace

type StdStreams =
  { stderr :: EventSource String
  , stdout :: EventSource String
  }

getChannels :: ManagedProcess -> Effect StdStreams
getChannels (ManagedProcess _ process _) = ado
  stdout <- onLine (Node.ChildProcess.stdout process) Just
  stderr <- onLine (Node.ChildProcess.stderr process) Just
  in { stdout, stderr }

-- Note: it will not throw, so to check the computation result
-- Fiber must be inspected.
redirectChannels
  :: StdStreams
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

type ClusterConfig r =
  ( ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , hooks :: Hooks
  | LogParams r
  )

-- | TODO: Replace original log params with the row type
type LogParams r =
  ( logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  | r
  )

-- Similar to `Aff.bracket`, except cleanup is pushed onto a stack to be run
-- later.
cleanupBracket
  :: forall (a :: Type) (b :: Type)
   . Ref (Array (Aff Unit))
  -> Aff a
  -> (a -> Aff Unit)
  -> (a -> Aff b)
  -> Aff b
cleanupBracket cleanupRef before after action = do
  Aff.bracket
    before
    (\res -> liftEffect $ Ref.modify_ ([ after res ] <> _) cleanupRef)
    action

mkLogging
  :: forall r
   . Record (LogParams r)
  -> Effect
       { updatedConfig :: Record (LogParams r)
       , logger :: Logger
       , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
       , printLogs :: Aff Unit
       , clearLogs :: Aff Unit
       }
mkLogging cfg
  | cfg.suppressLogs = ado
      -- if logs should be suppressed, setup the machinery and continue with
      -- the bracket
      { addLogEntry, suppressedLogger, printLogs, clearLogs } <-
        setupLogs cfg.logLevel cfg.customLogger
      let
        configLogger = Just $ map liftEffect <<< addLogEntry
      in
        { updatedConfig: cfg { customLogger = configLogger }
        , logger: suppressedLogger
        , customLogger: configLogger
        , printLogs: liftEffect printLogs
        , clearLogs: liftEffect clearLogs
        }
  | otherwise = pure
      -- otherwise, proceed with the env setup and provide a normal logger
      { updatedConfig: cfg
      , logger: mkLogger cfg.logLevel cfg.customLogger
      , customLogger: cfg.customLogger
      , printLogs: pure unit
      , clearLogs: pure unit
      }

makeNaiveClusterContractEnv
  :: forall r
   . Record (ClusterConfig r)
  -> Logger
  -> Maybe (LogLevel -> Message -> Aff Unit)
  -> Aff ContractEnv
makeNaiveClusterContractEnv cfg logger customLogger = do
  usedTxOuts <- newUsedTxOuts
  backend <- buildBackend logger $ mkCtlBackendParams
    { ogmiosConfig: cfg.ogmiosConfig
    , kupoConfig: cfg.kupoConfig
    }
  ledgerConstants <- getLedgerConstants
    cfg { customLogger = customLogger }
    backend
  backendKnownTxs <- liftEffect $ Ref.new Set.empty
  pure
    { backend
    , handle: mkQueryHandle cfg backend
    , networkId: TestnetId
    , logLevel: cfg.logLevel
    , customLogger: customLogger
    , suppressLogs: cfg.suppressLogs
    , hooks: cfg.hooks
    , wallet: Nothing
    , usedTxOuts
    , ledgerConstants
    -- timeParams have no effect when KeyWallet is used
    , timeParams: defaultTimeParams
    , synchronizationParams: defaultSynchronizationParams
    , knownTxs: { backend: backendKnownTxs }
    }

-- | Makes cluster ContractEnv with configured logs suppression and cleanup scheduled.
makeClusterContractEnv
  :: forall r
   . Ref (Array (Aff Unit))
  -> { updatedConfig :: Record (ClusterConfig r)
     , logger :: Logger
     , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
     , printLogs :: Aff Unit
     , clearLogs :: Aff Unit
     }
  -> Aff
       { env :: ContractEnv
       , clearLogs :: Aff Unit
       , printLogs :: Aff Unit
       }
makeClusterContractEnv
  cleanupRef
  { updatedConfig, logger, customLogger, printLogs, clearLogs } =
  cleanupBracket
    cleanupRef
    (makeNaiveClusterContractEnv updatedConfig logger customLogger)
    stopContractEnv
    (pure <<< { env: _, printLogs, clearLogs })

-- | Kill a process and wait for it to stop listening on a specific port.
stopChildProcessWithPort :: UInt -> ManagedProcess -> Aff Unit
stopChildProcessWithPort port childProcess = do
  stop childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)
