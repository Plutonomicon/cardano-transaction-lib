-- | This module provides an extensible interface for making various
-- | assertions about `Contract`s.
module Contract.Test.Utils (exitCode, interruptOnSignal) where

import Prelude

import Data.Posix.Signal (Signal)
import Data.Posix.Signal as Signal
import Effect (Effect)
import Effect.Aff (Fiber, killFiber, launchAff_)
import Effect.Exception (error)
import Node.Process as Process

--------------------------------------------------------------------------------
-- function to cancel aff fibers on signal
--------------------------------------------------------------------------------

foreign import exitCode :: Int -> Effect Unit

-- | attaches a custom handler on SIGINt to kill the fiber.
-- | see `doc/plutip-testing#custom-SIGINT-handlers`
interruptOnSignal :: forall a. Signal -> Fiber a -> Effect Unit
interruptOnSignal signal fiber = Process.onSignal signal do
  launchAff_ do
    killFiber (error $ Signal.toString signal) fiber
