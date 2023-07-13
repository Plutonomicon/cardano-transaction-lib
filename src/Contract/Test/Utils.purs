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

-- | Attaches a custom handler on SIGINT to kill the fiber.
-- | see https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md#note-on-sigint
interruptOnSignal :: forall a. Signal -> Fiber a -> Effect Unit
interruptOnSignal signal fiber = Process.onSignal signal do
  launchAff_ do
    killFiber (error $ Signal.toString signal) fiber
