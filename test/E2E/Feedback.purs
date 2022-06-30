-- | Store and retrieve values in simple JS variable (window.ctlTestFeedback),
-- | in order to establish a communication between the Examples and E2E tests.
-- | Retrieval must be called from Puppeteer, while publishing happens in the browser.
module Test.E2E.Feedback
  ( publishTestFeedback
  , retrieveTestFeedback
  , testFeedbackIsTrue
  ) where

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeFromForeign)
import Prelude
import Toppokki as Toki

foreign import _publishTestFeedback :: forall (a :: Type). a -> Effect Unit

-- | Store an arbitrary value as feedback
publishTestFeedback :: forall (a :: Type). a -> Aff Unit
publishTestFeedback = liftEffect <<< _publishTestFeedback

-- | Retrieve the feedback value
retrieveTestFeedback :: Toki.Page -> Aff Foreign
retrieveTestFeedback page = Toki.unsafeEvaluateStringFunction
  "window.ctlTestFeedback"
  page

-- | Convenience function for boolean feedback
testFeedbackIsTrue :: Toki.Page -> Aff Boolean
testFeedbackIsTrue page = unsafeFromForeign <$> retrieveTestFeedback page
