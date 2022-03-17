module Examples.Nami.WithDatums (main) where

import Prelude
import Undefined

import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Types.ByteArray (hexToByteArrayUnsafe)
import Types.Scripts (Validator)
import Types.Transaction (TransactionHash)

main :: Effect Unit
main = launchAff_ $ do
  undefined

alwaysSucceeds :: Aff (Maybe TransactionHash)
alwaysSucceeds = undefined

alwaysSucceedsValidator :: Validator
alwaysSucceedsValidator = wrap
  $ wrap
  $ hexToByteArrayUnsafe "4d01000033222220051200120011"
