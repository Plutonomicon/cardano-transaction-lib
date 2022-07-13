module Test.Parallel
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import QueryM (getChainTip, runQueryMWithSettings, traceQueryConfig)

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
  flip runQueryMWithSettings
    (getChainTip >>= liftEffect <<< Console.log <<< show) =<< traceQueryConfig
