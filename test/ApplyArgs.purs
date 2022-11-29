module Test.ApplyArgs (main) where

import Contract.Prelude
import Internal.ApplyArgs (greet)

main :: Effect Unit
main = do
    greet unit