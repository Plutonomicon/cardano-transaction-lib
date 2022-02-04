module Examples.Nami.Simple (main) where

import Prelude

import Effect (Effect)
import Effect.Console as Console

main :: Effect Unit
main = Console.log "Hello browser"
