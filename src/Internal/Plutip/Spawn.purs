-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Ctl.Internal.Plutip.Spawn
  ( NewOutputAction(NoOp, Success, Cancel)
  , ManagedProcess(ManagedProcess)
  , spawn
  , stop
  , waitForStop
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut) as AVar
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.AVar (isEmpty, read, status) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Node.ChildProcess
  ( ChildProcess
  , SpawnOptions
  , kill
  , stdout
  )
import Node.ChildProcess as ChildProcess
import Node.ReadLine (Interface, close, createInterface, setLineHandler) as RL
import Data.Foldable (fold)

-- | Carry along an `AVar` which resolves when the process closes.
-- | Necessary due to `child_process` having no way to query if a process has
-- | closed, so we must listen immediately after spawning.
data ManagedProcess = ManagedProcess String ChildProcess (AVar ChildProcess.Exit)

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
  closedAVar <- AVar.empty
  let mp = ManagedProcess (cmd <> fold ((" " <> _) <$> args)) child closedAVar
  interface <- RL.createInterface (stdout child) mempty
  outputRef <- Ref.new ""
  ChildProcess.onClose child \code -> do
    RL.close interface
    void $ AVar.tryPut code closedAVar
    output <- Ref.read outputRef
    cont $ Left $ error $
      "Process " <> cmd <> " exited. Output:\n" <> output

  -- Ideally we close the interface instead of detaching the listener
  -- but it seems to issue a close on the output stream of the process too
  -- which breaks some processes
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
                $ "Process cancelled because output received: " <> str
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
