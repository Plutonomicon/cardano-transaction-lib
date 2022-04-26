module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Integration as Integration
import Test.Unit as Unit

main :: Effect Unit
main = do
  Unit.main
  Integration.main
