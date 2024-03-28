module Test.Ctl.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import Cardano.Serialization.Lib (Costmdls, unpackMapContainerToMapWith)
import Cardano.Types (CostModel, Language)
import Cardano.Types.CostModel as CostModel
import Cardano.Types.Language as Language
import Data.Map (Map)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

foreign import defaultCostmdls :: Effect Costmdls

costModelsFixture1 :: Map Language CostModel
costModelsFixture1 = unsafePerformEffect do
  defaultCostmdls <#> unpackMapContainerToMapWith Language.fromCsl
      CostModel.fromCsl
