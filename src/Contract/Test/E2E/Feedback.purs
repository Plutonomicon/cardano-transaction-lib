-- | Store and retrieve values in simple JS variable (window.ctlTestFeedback),
-- | in order to establish a communication between the Examples and E2E tests.
-- | Retrieval must be called from Puppeteer, while publishing happens in the browser.
module CTL.Contract.Test.E2E.Feedback
  ( publishTestFeedback
  , retrieveTestFeedback
  , resetTestFeedback
  , testFeedbackIsTrue
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeFromForeign)
import Toppokki as Toppokki

foreign import _publishTestFeedback :: forall (a :: Type). a -> Effect Unit

-- | Store an arbitrary value as feedback
publishTestFeedback :: forall (a :: Type). a -> Aff Unit
publishTestFeedback = liftEffect <<< _publishTestFeedback

resetTestFeedback :: Toppokki.Page -> Aff Unit
resetTestFeedback = void <<< Toppokki.unsafeEvaluateStringFunction
  "window.ctlTestFeedback = false;"

-- | Retrieve the feedback value
retrieveTestFeedback :: Toppokki.Page -> Aff Foreign
retrieveTestFeedback = Toppokki.unsafeEvaluateStringFunction
  "window.ctlTestFeedback"

-- | Convenience function for boolean feedback
testFeedbackIsTrue :: Toppokki.Page -> Aff Boolean
testFeedbackIsTrue page = unsafeFromForeign <$> retrieveTestFeedback page
