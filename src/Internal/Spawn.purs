-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Ctl.Internal.Spawn
  ( NewOutputAction(NoOp, Success, Cancel)
  , OnSignalRef
  , ManagedProcess(ManagedProcess)
  , spawn
  , exec
  , stop
  , stopProcessWithChildren
  , waitForStop
  , cleanupTmpDir
  , cleanupOnSigint
  , removeOnSignal
  , waitForSignal
  , isPortAvailable
  , killProcessWithPort
  , _rmdirSync
  , FilePath
  ) where

import Contract.Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(Left))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Posix.Signal as Signal
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UInt (UInt)
import Data.UInt as UInt
import Debug (traceM)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut) as AVar
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff as Aff
import Effect.Aff.AVar (isEmpty, read, status) as AVar
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throw)
import Effect.Exception (message) as Error
import Effect.Ref as Ref
import Node.Buffer as Node.Buffer
import Node.ChildProcess (ChildProcess, SpawnOptions, kill, stderr, stdout)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as Node.ChildProcess
import Node.ReadLine (Interface, close, createInterface, setLineHandler) as RL
import Node.Stream as Node.Stream

-- | Carry along an `AVar` which resolves when the process closes.
-- | Necessary due to `child_process` having no way to query if a process has
-- | closed, so we must listen immediately after spawning.
data ManagedProcess = ManagedProcess String ChildProcess
  (AVar ChildProcess.Exit)

-- | Provides a way to react on update of a program output.
-- | Do nothing, indicate startup success, or thrown an exception to the Aff
-- | action, killing the spawned program.
data NewOutputAction = NoOp | Success | Cancel

-- | `spawn`, but with ability to wait for program startup, using a callback
-- | returning a `NewOutputAction`, or to kill the program depending on its
-- | output.
spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Maybe ({ output :: String, line :: String } -> Effect NewOutputAction)
  -> Aff ManagedProcess
spawn cmd args opts mbFilter =
  makeAff (spawn' cmd args opts mbFilter)

spawn'
  :: String
  -> Array String
  -> SpawnOptions
  -> Maybe ({ output :: String, line :: String } -> Effect NewOutputAction)
  -> (Either Error ManagedProcess -> Effect Unit)
  -> Effect Canceler
spawn' cmd args opts mbFilter cont = do
  child <- ChildProcess.spawn cmd args opts
  let fullCmd = cmd <> foldMap (" " <> _) args
  closedAVar <- AVar.empty
  stdoutInterfaceRef <- Ref.new Nothing
  stderrInterface <- RL.createInterface (stderr child) mempty
  flip RL.setLineHandler stderrInterface \str -> do
    traceM $ "stderr: " <> str
  outputRef <- Ref.new ""
  ChildProcess.onClose child \code -> do
    stdoutInterface <- Ref.read stdoutInterfaceRef
    traverse_ RL.close stdoutInterface
    RL.close stderrInterface
    void $ AVar.tryPut code closedAVar
    output <- Ref.read outputRef
    cont $ Left $ error
      $ "Process "
      <> fullCmd
      <> " exited. Output:\n"
      <> output

  -- Ideally we call `RL.close interface` instead of detaching the listener
  -- via `clearLineHandler interface`, but it causes issues with the output
  -- stream of some processes, namely `ogmios`. `ogmios` eventually freezes if
  -- we close the interface.
  let mp = ManagedProcess fullCmd child closedAVar
  case mbFilter of
    Nothing -> cont (pure mp)
    Just filter -> do
      stdoutInterface <- RL.createInterface (stdout child) mempty
      Ref.write (Just stdoutInterface) stdoutInterfaceRef
      flip RL.setLineHandler stdoutInterface
        \str -> do
          output <- Ref.modify (_ <> str <> "\n") outputRef
          filter { output, line: str } >>= case _ of
            Success -> do
              clearLineHandler stdoutInterface
              cont (pure mp)
            Cancel -> do
              kill SIGINT child
              clearLineHandler stdoutInterface
              cont $ Left $ error
                $ "Process cancelled because output received: "
                <> str
            _ -> pure unit

  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

exec
  :: String
  -> Aff
       { channels ::
           { stdout :: Node.Stream.Readable ()
           , stderr :: Node.Stream.Readable ()
           }
       , process :: Node.ChildProcess.ChildProcess
       }
exec cmd = Aff.makeAff \cont -> do
  processRef <- Ref.new Nothing
  isCanceledRef <- Ref.new false
  let
    isCanceled = Ref.read isCanceledRef
    markCanceled = Ref.write true isCanceledRef
  -- log $ show { exec: cmd }
  process <- Node.ChildProcess.exec
    cmd
    Node.ChildProcess.defaultExecOptions
    ( \{ error: err, stderr, stdout } -> isCanceled >>= flip unless do
        process <-
          liftMaybe (error "Couldn't find executed process" :: Error)
            =<< Ref.read processRef
        stderrStream <- readableFromBuffer stderr
        stdoutStream <- readableFromBuffer stdout
        let
          result =
            { channels: { stderr: stderrStream, stdout: stdoutStream }
            , process
            }
        cont $ maybe (Right result) Left err
    )
  Ref.write (Just process) processRef
  pure $ Aff.Canceler \err -> liftEffect do
    markCanceled
    cont $ Left err

foreign import readableFromBuffer
  :: Node.Buffer.Buffer -> Effect (Node.Stream.Readable ())

foreign import clearLineHandler :: RL.Interface -> Effect Unit

stop :: ManagedProcess -> Aff Unit
stop (ManagedProcess _ child closedAVar) = do
  isAlive <- AVar.isEmpty <$> AVar.status closedAVar
  when isAlive $ liftEffect $ kill SIGINT child

stopProcessWithChildren :: ManagedProcess -> Aff Unit
stopProcessWithChildren managedProc@(ManagedProcess _ proc _) = do
  void $ liftEffect $ Node.ChildProcess.execSync
    ("pkill -TERM -P " <> show (unwrap $ Node.ChildProcess.pid proc))
    Node.ChildProcess.defaultExecSyncOptions
  stop managedProc

-- | Waits until the process has cleanly stopped.
waitForStop :: ManagedProcess -> Aff Unit
waitForStop (ManagedProcess cmd _ closedAVar) = do
  AVar.read closedAVar >>= case _ of
    ChildProcess.Normally 0 -> pure unit
    _ -> throwError $ error $ "Process " <> cmd <> " did not exit cleanly"

foreign import data OnSignalRef :: Type

foreign import _rmdirSync :: FilePath -> Effect Unit

foreign import onSignalImpl :: String -> Effect Unit -> Effect OnSignalRef

foreign import removeOnSignal :: OnSignalRef -> Effect Unit

onSignal :: Signal -> Effect Unit -> Effect OnSignalRef
onSignal sig = onSignalImpl (Signal.toString sig)

-- | Just as onSignal, but Aff.
waitForSignal :: Signal -> Aff Signal
waitForSignal signal = makeAff \cont -> do
  isCanceledRef <- Ref.new false
  onSignalRef <- onSignal signal
    $ Ref.read isCanceledRef
    >>= flip unless (cont $ Right signal)
  pure $ Canceler \err -> liftEffect do
    Ref.write true isCanceledRef
    removeOnSignal onSignalRef
    cont $ Left err

cleanupOnSigint :: FilePath -> FilePath -> Effect OnSignalRef
cleanupOnSigint workingDir testClusterDir = do
  sig <- onSignal SIGINT do
    _rmdirSync workingDir
    _rmdirSync testClusterDir
  pure sig

killByPort :: UInt -> Effect Unit
killByPort port =
  void $ Node.ChildProcess.exec
    ("fuser -k " <> show (UInt.toInt port) <> "/tcp")
    Node.ChildProcess.defaultExecOptions
    (maybe (pure unit) (log <<< Error.message) <<< _.error)

-- | Kill a process and wait for it to stop listening on a specific port.
killProcessWithPort :: UInt -> Aff Unit
killProcessWithPort port = do
  liftEffect $ killByPort port
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)

cleanupTmpDir :: ManagedProcess -> FilePath -> Effect Unit
cleanupTmpDir (ManagedProcess _ child _) workingDir = do
  ChildProcess.onExit child \_ -> do
    _rmdirSync workingDir

type FilePath = String

foreign import _isPortAvailable :: Int -> Effect (Promise Boolean)

isPortAvailable :: UInt -> Aff Boolean
isPortAvailable = toAffE <<< _isPortAvailable <<< UInt.toInt
