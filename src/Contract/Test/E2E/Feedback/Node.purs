module Contract.Test.E2E.Feedback.Node
  ( getBrowserEvents
  , subscribeToBrowserEvents
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Contract.Test.E2E.Feedback (BrowserEvent(Failure, Success))
import Control.Monad.Rec.Class (forever)
import Control.Parallel (parallel, sequential)
import Data.Either (hush, note)
import Data.Maybe (Maybe, fromMaybe)
import Data.Number (infinity)
import Data.Time.Duration (Seconds(Seconds), fromDuration)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds), delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Foreign (unsafeFromForeign)
import Helpers (liftEither)
import Toppokki as Toppokki

-- | React to events raised by the browser
subscribeToBrowserEvents
  :: forall a
   . DecodeAeson a
  => Maybe Seconds
  -> Toppokki.Page
  -> (BrowserEvent a -> Effect Unit)
  -> Aff Unit
subscribeToBrowserEvents timeout page cont = do
  sequential ado
    parallel waitForTimeout
    parallel process
    in unit
  where
  waitForTimeout = do
    delay $ fromDuration (fromMaybe (Seconds infinity) timeout)
    liftEffect $ throw "Timeout reached"
  process = do
    void $ try $ forever do
      events <- getBrowserEvents page
      for_ events \event -> do
        void $ liftEffect $ try $ cont event
        case event of
          Success -> liftEffect $ throw "Success"
          Failure _ -> liftEffect $ throw "Failure"
          _ -> pure unit
      delay $ Milliseconds $ 1000.0

getBrowserEvents
  :: forall a. DecodeAeson a => Toppokki.Page -> Aff (Array (BrowserEvent a))
getBrowserEvents page = do
  frgn <- Toppokki.unsafeEvaluateStringFunction collectEventsJS page
  let
    (encodedEvents :: Array String) = unsafeFromForeign frgn
  for encodedEvents \event -> do
    liftEither $ note (error $ "Unable to decode BrowserEvent from: " <> event)
      $ hush
      $ decodeAeson =<< parseJsonStringToAeson event

collectEventsJS :: String
collectEventsJS =
  """
  (() => {
    const res = window.ctlE2ECommunications || [];
    window.ctlE2ECommunications = [];
    return res;
  })()
  """
