module Test.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Cardano.Types.Transaction (Costmdls)
import Control.Monad.Error.Class (liftEither)
import Effect.Unsafe (unsafePerformEffect)
import Serialization (defaultCostmdls)
import Deserialization.Transaction (convertCostModels)
import Deserialization.Error (toError)

costModelsFixture1 :: Costmdls
costModelsFixture1 = unsafePerformEffect
  (defaultCostmdls >>= convertCostModels >>> lmap toError >>> liftEither)
