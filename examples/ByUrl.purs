module Examples.ByUrl (main) where

import Prelude
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Array (last)
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Examples.AlwaysMints as AlwaysMints
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.Datums as Datums
import Examples.Nami as Nami
import Examples.Gero as Gero
import Examples.Pkh2Pkh as Pkh2Pkh
import Examples.SignMultiple as SignMultiple

foreign import _queryString :: Effect String

main :: Effect Unit
main = last <<< split (Pattern "?") <$> _queryString >>=
  case _ of
    Just "AlwaysMints" -> AlwaysMints.main
    Just "AlwaysSucceeds" -> AlwaysSucceeds.main
    Just "Datums" -> Datums.main
    Just "Nami" -> Nami.main
    Just "Gero" -> Gero.main
    Just "Pkh2Pkh" -> Pkh2Pkh.main
    Just "SignMultiple" -> SignMultiple.main
    _ -> liftEffect $ error "Error parsing query string"
