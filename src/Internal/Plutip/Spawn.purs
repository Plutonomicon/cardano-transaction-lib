-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Ctl.Internal.Plutip.Spawn
  ( NewOutputAction(NoOp, Success, Cancel)
  , OnSignalRef
  , ManagedProcess(ManagedProcess)
  , spawn
  , stop
  , waitForStop
  , cleanupTmpDir
  , cleanupOnSigint
  , removeOnSignal
  , waitForSignal
  , killProcessWithPort
  , _rmdirSync
  ) where

import Contract.Prelude

import Control.Monad.Error.Class (throwError)
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Ctl.Internal.Plutip.Types (FilePath)
import Data.Either (Either(Left))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Posix.Signal as Signal
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut) as AVar
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.AVar (isEmpty, read, status) as AVar
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Node.ChildProcess
  ( ChildProcess
  , SpawnOptions
  , kill
  , stdout
  )
import Node.ChildProcess as ChildProcess
import Node.ChildProcess as Node.ChildProcess
import Node.ReadLine (Interface, close, createInterface, setLineHandler) as RL

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
  -> Maybe (String -> NewOutputAction)
  -> Aff ManagedProcess
spawn cmd args opts mbFilter =
  makeAff (spawn' cmd args opts mbFilter)

spawn'
  :: String
  -> Array String
  -> SpawnOptions
  -> Maybe (String -> NewOutputAction)
  -> (Either Error ManagedProcess -> Effect Unit)
  -> Effect Canceler
spawn' cmd args opts mbFilter cont = do
  child <- ChildProcess.spawn cmd args opts
  let fullCmd = cmd <> foldMap (" " <> _) args
  closedAVar <- AVar.empty
  interface <- RL.createInterface (stdout child) mempty
  outputRef <- Ref.new ""
  ChildProcess.onClose child \code -> do
    RL.close interface
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
      flip RL.setLineHandler interface
        \str -> do
          output <- Ref.modify (_ <> str <> "\n") outputRef
          case filter output of
            Success -> do
              clearLineHandler interface
              cont (pure mp)
            Cancel -> do
              kill SIGINT child
              clearLineHandler interface
              cont $ Left $ error
                $ "Process cancelled because output received: "
                <> str
            _ -> pure unit

  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

foreign import clearLineHandler :: RL.Interface -> Effect Unit

stop :: ManagedProcess -> Aff Unit
stop (ManagedProcess _ child closedAVar) = do
  isAlive <- AVar.isEmpty <$> AVar.status closedAVar
  when isAlive $ liftEffect $ kill SIGINT child

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
waitForSignal :: Signal -> Aff Unit
waitForSignal signal = makeAff \cont -> do
  isCanceledRef <- Ref.new false
  onSignalRef <- onSignal signal
    $ Ref.read isCanceledRef >>= flip unless (cont $ Right unit)
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
    \{ error } -> maybe (pure unit) throwError error

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
