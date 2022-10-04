-- | This module provides ability to spawn a program using `spawn` and wait
-- | for some specific output that indicates that the program has started
-- | successfully or failed, in which case an exception is thrown.
module Ctl.Internal.Plutip.Spawn
  ( NewOutputAction(NextLine, Success)
  , Stopped
  , spawn
  , spawnAndWaitForOutput
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (empty, take, read) as AVar
import Effect.AVar (put, tryPut) as Effect.AVar
import Effect.Class (liftEffect)
import Node.ChildProcess
  ( ChildProcess
  , SpawnOptions
  , stdout
  , onClose
  )
import Node.ChildProcess (spawn) as Child
import Node.ReadLine (createInterface, setLineHandler, Interface) as RL

-- | Provides a way to react on update of a program output.
-- | Do nothing, indicate startup success, or thrown an exception to the Aff
-- | action, killing the spawned program.
data NewOutputAction = NextLine | Success

type Stopped = Aff Unit

-- | `spawn` but also returns an action to wait for the process to stop
spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Aff (ChildProcess /\ Stopped)
spawn cmd args opts = do
  child <- liftEffect $ Child.spawn cmd args opts
  closedAVar <- AVar.empty
  liftEffect $ onClose child $ const do
    void $ Effect.AVar.tryPut unit closedAVar
  pure $ child /\ AVar.read closedAVar

-- | `spawn`, but with ability to wait for program startup, using a callback
-- | returning a `NewOutputAction`, or to kill the program depending on its
-- | output.
spawnAndWaitForOutput
  :: String
  -> Array String
  -> SpawnOptions
  -> (String -> Aff NewOutputAction)
  -> Aff (ChildProcess /\ Stopped)
spawnAndWaitForOutput cmd args opts filter = do
  child /\ stopped <- spawn cmd args opts
  interface <- liftEffect $ RL.createInterface (stdout child) mempty
  lineAVar <- AVar.empty
  let
    listen = liftEffect $ flip RL.setLineHandler interface \line ->
      Effect.AVar.put line lineAVar (const $ pure unit)
    unlisten _ = liftEffect $ clearLineHandler interface
    go = AVar.take lineAVar >>= filter >>= case _ of
      Success -> pure unit
      NextLine -> go
  bracket listen unlisten (const go)
  pure $ child /\ stopped

foreign import clearLineHandler :: RL.Interface -> Effect Unit
