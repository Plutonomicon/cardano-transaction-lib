module Test.Parallel
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import QueryM (getChainTip, runQueryM)
import QueryM.Config (testnetTraceQueryConfig)

main :: Effect Unit
main = do
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip
  launchAff_ chainTip

chainTip :: Aff Unit
chainTip = do
  runQueryM testnetTraceQueryConfig
    (getChainTip >>= show >>> Console.log >>> liftEffect)
