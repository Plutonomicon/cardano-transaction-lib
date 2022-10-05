-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Ctl.Internal.Plutip.Spawn
  ( NewOutputAction(NextLine, Success)
  , ManagedProcess
  , spawn
  , waitForOutput
  , waitForStop
  , stop
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, try)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(Left, Right))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.AVar (put, tryPut) as Effect.AVar
import Effect.Aff (Aff, bracket, error, throwError)
import Effect.Aff.AVar (empty, read, take) as AVar
import Effect.Class (liftEffect)
import Node.ChildProcess
  ( ChildProcess
  , Exit(Normally)
  , SpawnOptions
  , kill
  , onClose
  , stdout
  )
import Node.ChildProcess (spawn) as Child
import Node.ReadLine (Interface, createInterface, setLineHandler) as RL

-- | Provides a way to react on update of a program output.
-- | Do nothing, indicate startup success, or thrown an exception to the Aff
-- | action, killing the spawned program.
data NewOutputAction = NextLine | Success

-- | Carry along an `Aff` action which resolves when the process closes.
-- | Necessary due to `child_process` having no way to query if a process has
-- | closed, so we must listen immediately after spawning.
data ManagedProcess = ManagedProcess ChildProcess (Aff Unit)

spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Aff ManagedProcess
spawn cmd args opts = do
  child <- liftEffect $ Child.spawn cmd args opts
  closedAVar <- AVar.empty
  liftEffect $ onClose child $ \code -> do
    void $ Effect.AVar.tryPut code closedAVar
  pure $ ManagedProcess child $ AVar.read closedAVar >>= case _ of
    Normally 0 -> pure unit
    _ -> throwError $ error "Process did not exit cleanly"

waitForStop :: ManagedProcess -> Aff Unit
waitForStop (ManagedProcess _ stopped) = stopped

stop :: ManagedProcess -> Aff Unit
stop (ManagedProcess child _) = liftEffect $ kill SIGINT child

-- | `ManagedProcess` combinator to wait for program startup, using a callback
-- | returning a `NewOutputAction`, or to kill the program depending on its
-- | output.
waitForOutput
  :: (String -> Aff NewOutputAction)
  -> ManagedProcess
  -> Aff ManagedProcess
waitForOutput filter mp@(ManagedProcess child _) = do
  interface <- liftEffect $ RL.createInterface (stdout child) mempty
  lineAVar <- AVar.empty
  let
    listen = liftEffect $ flip RL.setLineHandler interface \line ->
      Effect.AVar.put line lineAVar (const $ pure unit)
    unlisten _ = liftEffect $ clearLineHandler interface
    go = AVar.take lineAVar >>= filter >>> try >>= case _ of
      Right Success -> pure $ Right unit
      Right NextLine -> go
      Left e -> pure (Left e)
  bracket listen unlisten \_ -> liftEither =<< do
    sequential $ parallel go <|> parallel
      ( try (waitForStop mp) $> Left
          (error "Process stopped while waiting for output")
      )
  pure mp

foreign import clearLineHandler :: RL.Interface -> Effect Unit
