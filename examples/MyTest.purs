module Ctl.Examples.MyTest (main) where

import Contract.Monad (launchAff_)
import Contract.Prelude (Effect, liftEffect, log, wrap)
import Data.Array (take)
import Effect.Aff (delay)
import Prelude (Unit, bind, discard, pure, unit, ($), (+), (<>))

main :: Effect Unit
main = do
  log "Hello, World!"
  repeatedlyCreatePlutusData
  pure unit

repeatedlyCreatePlutusData :: Effect Unit
repeatedlyCreatePlutusData = launchAff_ $ aux [] 0
  where
  aux _ 20 = pure unit
  aux arr cnt = do
    a <- liftEffect $ loopEffect
    let newArr = take 10 $ a <> arr
    delay $ wrap $ 100.0
    aux newArr (cnt + 1)

foreign import loopEffect :: Effect (Array Type)
