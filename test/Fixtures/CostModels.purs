module Test.Ctl.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Cardano.Types.Transaction (Costmdls)
import Ctl.Internal.Deserialization.Error (toError)
import Ctl.Internal.Deserialization.Transaction (convertCostModels)
import Ctl.Internal.Serialization (defaultCostmdls)
import Data.Bifunctor (lmap)
import Effect.Unsafe (unsafePerformEffect)

costModelsFixture1 :: Costmdls
costModelsFixture1 = unsafePerformEffect
  (defaultCostmdls >>= convertCostModels >>> lmap toError >>> liftEither)
