module Test.E2E.Helpers (retrieveJQuery) where

import Toppokki as Toki
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect (Effect)
import Prelude
import Control.Promise (Promise, toAffE)

foreign import _retrieveJQuery :: Toki.Page -> Effect (Promise String)

retrieveJQuery :: Toki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery
