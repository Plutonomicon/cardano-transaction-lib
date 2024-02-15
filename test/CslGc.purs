module Test.Ctl.CslGc
  ( suite
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Effect (Effect)
import Effect.Aff (Aff)
import Mote (group, test)

foreign import testExternalMemLeakImpl
  :: CslGcTestConfig -> Effect (Promise Unit)

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "CSL Garbage Collection" do
    test "External memory does not leak" do
      testExternalMemLeak
        { numIterations: 15
        , refIteration: 5
        , maxError: 50
        , delay: 1200
        , numArrays: 20
        , arrSize: 1_000_000
        }

type CslGcTestConfig =
  { numIterations :: Int
  , refIteration :: Int
  , maxError :: Int -- percent
  , delay :: Int -- msec
  , numArrays :: Int
  , arrSize :: Int
  }

-- We consider the test successfull if external memory consumption
-- stops growing after i-th iteration.
--
-- Note, that wasm has no support for memory shrinking, i.e. once
-- the allocated memory is freed, it will be reused in subsequent
-- wasm allocations, but will not be returned to the process.
testExternalMemLeak :: CslGcTestConfig -> Aff Unit
testExternalMemLeak = toAffE <<< testExternalMemLeakImpl
