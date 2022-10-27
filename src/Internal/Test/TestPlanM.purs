module Ctl.Internal.Test.TestPlanM
  ( TestPlanM
  , interpret
  , interpretWithTimeout
  , interpretWithConfig
  ) where

import Prelude

import Ctl.Internal.Test.ConsoleReporter (consoleReporter)
import Data.Const (Const)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Mote (MoteT, Plan, foldPlan, planT)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Spec.Runner as SpecRunner

type TestPlanM :: Type -> Type -> Type
type TestPlanM test a = MoteT (Const Void) test Aff a

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM (Aff Unit) Unit -> Aff Unit
interpret = interpretWithConfig defaultConfig { timeout = Just (wrap 50000.0) }

interpretWithTimeout
  :: Maybe Milliseconds -> TestPlanM (Aff Unit) Unit -> Aff Unit
interpretWithTimeout timeout spif = do
  interpretWithConfig (defaultConfig { timeout = timeout }) spif

interpretWithConfig
  :: SpecRunner.Config -> TestPlanM (Aff Unit) Unit -> Aff Unit
interpretWithConfig config spif = do
  plan <- planT spif
  runSpec' config [ consoleReporter ] $ planToSpec plan

planToSpec :: Plan (Const Void) (Aff Unit) -> Spec Unit
planToSpec =
  foldPlan
    (\x -> it x.label $ liftAff x.value)
    pending
    (\x -> describe x.label $ planToSpec x.value)
    sequence_
