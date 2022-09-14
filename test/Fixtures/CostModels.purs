module Test.CTL.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import Data.Bifunctor (lmap)
import CTL.Internal.Cardano.Types.Transaction (Costmdls)
import Control.Monad.Error.Class (liftEither)
import Effect.Unsafe (unsafePerformEffect)
import CTL.Internal.Serialization (defaultCostmdls)
import CTL.Internal.Deserialization.Transaction (convertCostModels)
import CTL.Internal.Deserialization.Error (toError)

costModelsFixture1 :: Costmdls
costModelsFixture1 = unsafePerformEffect
  (defaultCostmdls >>= convertCostModels >>> lmap toError >>> liftEither)
