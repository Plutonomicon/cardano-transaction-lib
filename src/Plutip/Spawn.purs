-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Plutip.Spawn
  ( NewOutputAction(NextLine, Success)
  , spawnAndWaitForOutput
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (empty, take) as AVar
import Effect.AVar (put) as Effect.AVar
import Effect.Class (liftEffect)
import Node.ChildProcess
  ( ChildProcess
  , SpawnOptions
  , spawn
  , stdout
  )
import Node.ReadLine (createInterface, setLineHandler, Interface) as RL

-- | Provides a way to react on update of a program output.
-- | Do nothing, indicate startup success, or thrown an exception to the Aff
-- | action, killing the spawned program.
data NewOutputAction = NextLine | Success

-- | `spawn`, but with ability to wait for program startup, using a callback
-- | returning a `NewOutputAction`, or to kill the program depending on its
-- | output.
spawnAndWaitForOutput
  :: forall (m :: Type -> Type)
   . String
  -> Array String
  -> SpawnOptions
  -> (String -> Aff NewOutputAction)
  -> Aff ChildProcess
spawnAndWaitForOutput cmd args opts filter = do
  child <- liftEffect $ spawn cmd args opts
  interface <- liftEffect $ RL.createInterface (stdout child) mempty
  lineAVar <- AVar.empty
  let
    listen = liftEffect $ flip RL.setLineHandler interface \line -> Effect.AVar.put line lineAVar (const $ pure unit) 
    unlisten _ = liftEffect $ clearLineHandler interface
    go = AVar.take lineAVar >>= filter >>= case _ of
      Success -> pure unit
      NextLine -> go
  bracket listen unlisten (const go)
  pure child

foreign import clearLineHandler :: RL.Interface -> Effect Unit
