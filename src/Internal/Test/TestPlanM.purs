module Ctl.Internal.Test.TestPlanM
  ( TestPlanM
  , interpret
  , interpretWithConfig
  ) where

import Prelude

import Ctl.Internal.Test.ConsoleReporter (consoleReporter)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (wrap)
import Effect.Aff (Aff, bracket)
import Mote (MoteT, Plan, foldPlan, planT)
import Mote.Entry (Bracket, unBracket)
import Test.Spec (SpecT, afterAll, beforeAll, beforeWith, describe, it, pending)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Spec.Runner as SpecRunner

type TestPlanM :: Type -> Type -> Type
type TestPlanM test a = MoteT Aff test Aff a

type AffSpec :: Type -> Type
type AffSpec a = SpecT Aff Unit Aff a

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM (Aff Unit) Unit -> Aff Unit
interpret = interpretWithConfig defaultConfig { timeout = Just (wrap 50000.0) }

interpretWithConfig
  :: SpecRunner.Config -> TestPlanM (Aff Unit) Unit -> Aff Unit
interpretWithConfig config spif = do
  plan <- planT spif
  void $ join $ runSpecT config [ consoleReporter ] $ planToSpec plan

planToSpec :: Plan Aff (Aff Unit) -> AffSpec Unit
planToSpec =
  foldPlan
    (\x -> it x.label $ runBracket x.value x.bracket)
    pending
    (\x -> describe x.label $ runBracketSpec (planToSpec x.value) x.bracket)
    sequence_
  where
  runBracketSpec :: AffSpec Unit -> Maybe (Bracket Aff) -> AffSpec Unit
  runBracketSpec action = maybe action
    $ unBracket \before after -> do
        -- Use beforeWith to ignore the existential argument and supply the expected unit
        beforeAll before
          (afterAll after (beforeWith (const $ pure unit) action))

  runBracket :: Aff Unit -> Maybe (Bracket Aff) -> Aff Unit
  runBracket action = maybe action
    $ unBracket \before after -> bracket before after (const action)
