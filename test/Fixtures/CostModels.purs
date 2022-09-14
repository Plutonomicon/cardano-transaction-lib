module Test.CTL.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import CTL.Internal.Cardano.Types.Transaction (Costmdls)
import CTL.Internal.Deserialization.Error (toError)
import CTL.Internal.Deserialization.Transaction (convertCostModels)
import CTL.Internal.Serialization (defaultCostmdls)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Effect.Unsafe (unsafePerformEffect)

costModelsFixture1 :: Costmdls
costModelsFixture1 = unsafePerformEffect
  (defaultCostmdls >>= convertCostModels >>> lmap toError >>> liftEither)
