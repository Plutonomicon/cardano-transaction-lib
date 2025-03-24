module Test.Ctl.Fixtures.CostModels
  ( costModelsFixture1
  ) where

import Prelude

import Cardano.Data.Lite (Costmdls, unpackMapContainerToMapWith)
import Cardano.Types (CostModel, Language)
import Cardano.Types.CostModel as CostModel
import Cardano.Types.Language as Language
import Data.Map (Map)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

foreign import defaultCostmdls :: Effect Costmdls
foreign import plutusAlonzoCostmdls :: Effect Costmdls
foreign import plutusVasilCostmdls :: Effect Costmdls
foreign import plutusConwayCostmdls :: Effect Costmdls

costModelsFixture1 :: Map Language CostModel
costModelsFixture1 = unsafePerformEffect do
  defaultCostmdls <#> unpackMapContainerToMapWith Language.fromCdl
    CostModel.fromCdl
