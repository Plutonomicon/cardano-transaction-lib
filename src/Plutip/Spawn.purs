-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Plutip.Spawn
  ( NewOutputAction(NoOp, Success, Cancel)
  , spawnAndWaitForOutput
  ) where

import Prelude

import Data.Either (Either(Left))
import Data.Posix.Signal (Signal(SIGINT))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Node.ChildProcess
  ( ChildProcess
  , SpawnOptions
  , kill
  , onExit
  , spawn
  , stdout
  )
import Node.Encoding as Encoding
import Node.Stream (onDataString)

-- | Provides a way to react on update of a program output.
-- | Do nothing, indicate startup success, or thrown an exception to the Aff
-- | action, killing the spawned program.
data NewOutputAction = NoOp | Success | Cancel

-- | `spawn`, but with ability to wait for program startup, using a callback
-- | returning a `NewOutputAction`, or to kill the program depending on its
-- | output.
spawnAndWaitForOutput
  :: String
  -> Array String
  -> SpawnOptions
  -> (String -> NewOutputAction)
  -> Aff ChildProcess
spawnAndWaitForOutput cmd args opts filter =
  makeAff (spawnAndWait cmd args opts filter)

spawnAndWait
  :: String
  -> Array String
  -> SpawnOptions
  -> (String -> NewOutputAction)
  -> (Either Error ChildProcess -> Effect Unit)
  -> Effect Canceler
spawnAndWait cmd args opts filter cont = do
  child <- spawn cmd args opts
  onExit child $ const $ cont $ Left $ error $ "Process " <> cmd <> " exited"
  ref <- Ref.new ""
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      Ref.modify_ (_ <> str) ref
      output <- Ref.read ref
      case filter output of
        NoOp -> pure unit
        Success -> cont (pure child)
        Cancel -> do
          kill SIGINT child
          cont $ Left $ error
            $ "Process cancelled because output received: " <> str
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child
