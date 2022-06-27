module Examples.TestFeedback
       ( publishTestFeedback
       , retrieveTestFeedback
       ) where

import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, toAffE)
import Prelude (Unit, (<<<), ($))
import Data.Maybe (Maybe(..))

foreign import _publishTestFeedback :: forall (a :: Type). a -> Effect Unit
foreign import _retrieveTestFeedback :: forall (a :: Type). (Maybe a) -> (a -> Maybe a) -> Effect (Promise (Maybe a))

publishTestFeedback :: forall (a :: Type). a -> Aff Unit
publishTestFeedback = liftEffect <<< _publishTestFeedback

retrieveTestFeedback :: forall (a :: Type). Aff (Maybe a)
retrieveTestFeedback = toAffE $ _retrieveTestFeedback Nothing Just
