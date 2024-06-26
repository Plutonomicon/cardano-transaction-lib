module Ctl.Internal.Testnet.Server
  ( Channels
  , StartedTestnetCluster(MkStartedTestnetCluster)
  , startTestnetCluster
  ) where

import Contract.Prelude

import Control.Alt ((<|>))
import Control.Apply (applySecond)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(Loop), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Server
  ( startKupo
  , startOgmios
  , stopChildProcessWithPort
  )
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess(ManagedProcess)
  , NewOutputAction(NoOp, Success)
  , _rmdirSync
  , killProcessWithPort
  , spawn
  )
import Ctl.Internal.Plutip.Utils
  ( EventSource
  , addCleanup
  , annotateError
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
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , KupmiosConfig
  , Node
  , TestnetPaths
  )
import Ctl.Internal.Testnet.Utils
  ( findNodeDirs
  , findTestnetPaths
  , getRuntime
  , readNodes
  )
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix, trim) as String
import Data.Time.Duration (Milliseconds(Milliseconds))
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Foreign.Object as Object
import Node.ChildProcess (defaultSpawnOptions)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as Node.FS
import Node.Path (FilePath)
import Node.Process as Node.Process

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

  log "startTestnetCluster:done"
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
        (stopChildProcessWithPort startupParams.kupoConfig.port <<< fst)

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
        (startOgmios startupParams paths)
        (stopChildProcessWithPort startupParams.ogmiosConfig.port)

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

-- | Runs cardano-testnet executable with provided params.
spawnCardanoTestnet
  :: forall r
   . { cwd :: FilePath }
  -> { | CardanoTestnetStartupParams r }
  -> Aff { testnet :: ManagedProcess, workspace :: FilePath }
spawnCardanoTestnet { cwd } params = do
  env <- liftEffect Node.Process.getEnv
  -- initCwd <- liftMaybe (error "Couldn't find INIT_CWD env variable")
  --   $ Object.lookup "INIT_CWD" env
  let
    env' = Object.fromFoldable
      [ "TMPDIR" /\ cwd -- only for 8.1.1; 8.7.2 puts it's testnet directory into cwd instead
      -- , "CARDANO_NODE_SRC" /\ (initCwd <</>> "cardano-testnet-files")
      , "CARDANO_CLI" /\ "cardano-cli"
      , "CREATE_SCRIPT_CONTEXT" /\ "create-script-context"
      , "CARDANO_NODE" /\ "cardano-node"
      , "CARDANO_SUBMIT_API" /\ "cardano-submit-api"
      , "CARDANO_NODE_CHAIRMAN" /\ "cardano-node-chairman"
      ]
    opts = defaultSpawnOptions
      { cwd = Just cwd, env = Just $ Object.union env' env }
  workspaceRef <- liftEffect $ Ref.new mempty
  ps <- spawn "cardano-testnet" (spy "cardano-testnet options: " options) opts $
    Just
      ( \{ line } ->
          case String.stripPrefix (Pattern "Workspace: ") (String.trim line) of
            Nothing -> pure NoOp
            Just workspace -> do
              void $ Ref.write workspace workspaceRef
              pure Success
      )
  workspace <- liftEffect $ Ref.read workspaceRef
  pure { testnet: ps, workspace }
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
  tmpDir <- liftEffect tmpdir
  { testnet, workspace } <- spawnCardanoTestnet { cwd: tmpDir } params
  channels <- liftEffect $ getChannels testnet

  void $ Aff.forkAff $ annotateError "startCardanoTestnet:waitForErrorOrClose"
    do
      let
        waitError = Just <$> waitForError testnet
        waitClose = Nothing <$ waitForClose testnet
      cause <- waitError <|> waitClose
      runCleanup cleanupRef
      throwError $ fromMaybe (error "cardano-testnet process has exited") cause

  nodes <-
    waitUntil (Milliseconds 3000.0) $ liftEffect do
      hush <$> tryAndLogErrors "startCardanoTestnet:waitForNodes" do
        nodeDirs <- findNodeDirs { workdir: workspace }
        readNodes { testnetDirectory: workspace, nodeDirs }

  liftEffect $
    for_ nodes \{ port } ->
      addCleanup cleanupRef (killProcessWithPort port)

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
          _rmdirSync workspace

  _ <- redirectChannels
    { stderr: channels.stderr, stdout: channels.stdout }
    { stdoutTo:
        { log: Just $ workspace <</>> "cardano-testnet.stdout.log"
        , console: Nothing
        }
    , stderrTo:
        { log: Just $ workspace <</>> "cardano-testnet.stderr.log"
        , console: Nothing
        }
    }

  log "startCardanoTestnet:done"
  pure { testnet, workdirAbsolute: workspace, channels, nodes }

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
