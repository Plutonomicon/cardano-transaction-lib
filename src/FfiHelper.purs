module FFiHelpers (MaybeFfiHelper, maybeFfiHelper) where

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

type MaybeFfiHelper = { nothing :: forall x. Maybe x, just :: forall x. x -> Maybe x, from :: forall x. x -> Maybe x -> x }

maybeFfiHelper :: MaybeFfiHelper
maybeFfiHelper = { nothing: Nothing, just: Just, from: fromMaybe }
