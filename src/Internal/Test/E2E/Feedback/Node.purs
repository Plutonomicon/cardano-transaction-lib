-- | A module for communication between E2E test suite and a headless browser.
-- | This module exposes API for the NodeJS side.
-- | See `Ctl.Internal.Test.E2E.Feedback.Browser` for the corresponding APIs
-- | for the NodeJS side.
module Ctl.Internal.Test.E2E.Feedback.Node
  ( getBrowserEvents
  , subscribeToBrowserEvents
  ) where

import Prelude

import Aeson (decodeAeson, parseJsonStringToAeson)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Test.E2E.Feedback (BrowserEvent(Failure, Success))
import Data.Array (all)
import Data.Array as Array
import Data.Either (Either(Left), hush, note)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Number (infinity)
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (for, traverse_)
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , Milliseconds(Milliseconds)
  , delay
  , forkAff
  , killFiber
  , launchAff_
  , makeAff
  , try
  )
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import Foreign (unsafeFromForeign)
import Toppokki as Toppokki

-- | React to events raised by the browser
subscribeToBrowserEvents
  :: Maybe Seconds
  -> Toppokki.Page
  -> (BrowserEvent -> Effect Unit)
  -> Aff Unit
subscribeToBrowserEvents timeout page cont = do
  logs <- liftEffect $ Ref.new ""
  let
    addLogLine line = Ref.modify_ (flip append (line <> "\n")) logs
  liftEffect $ Toppokki.onConsole
    ( mkEffectFn1 \cm -> launchAff_ do
        Toppokki.consoleMessageText cm >>= liftEffect <<< addLogLine
    )
    page
  makeAff \f -> do
    liftEffect $ Toppokki.onPageError
      ( mkEffectFn1
          ( \err -> do
              allLogs <- Ref.read logs
              Console.log allLogs
              f $ Left err
          )
      )
      page
    let
      -- Accepts a number of attempts left.
      -- An attempt is successful if we get at least one event.
      process :: Maybe Int -> Aff Unit
      process attempts = do
        events <- getBrowserEvents page
        continue <- for events \event -> do
          void $ liftEffect $ try $ cont event
          case event of
            Success -> pure false
            Failure err -> liftEffect $ throw err
            _ -> pure true
        if all identity continue then do
          delay $ Milliseconds $ 1000.0
          if Array.length events == 0 && attempts /= Just 0 then
            process (flip sub one <$> attempts)
          else if attempts == Just 0 then liftEffect $ f $ Left $ error
            "Timeout reached when trying to connect to CTL Contract running\
            \ in the browser. Is there a Contract with E2E hooks available\
            \ at the URL you provided?"
          else process Nothing
        else pure unit

    timeoutFiber <- Ref.new Nothing
    processFiber <- Ref.new Nothing
    launchAff_ do
      liftEffect <<< flip Ref.write timeoutFiber <<< Just =<< forkAff do
        delay $ wrap $ 1000.0 * unwrap (fromMaybe (Seconds infinity) timeout)
        liftEffect $ f $ Left $ error "Timeout reached"
      liftEffect <<< flip Ref.write processFiber <<< Just =<< forkAff do
        try (process (Just firstTimeConnectionAttempts)) >>= liftEffect <<< f
    pure $ Canceler \e -> do
      liftEffect (Ref.read timeoutFiber) >>= traverse_ (killFiber e)
      liftEffect (Ref.read timeoutFiber) >>= traverse_ (killFiber e)
  where
  -- How many times to try until we get any event?
  firstTimeConnectionAttempts :: Int
  firstTimeConnectionAttempts = 10

getBrowserEvents
  :: Toppokki.Page -> Aff (Array BrowserEvent)
getBrowserEvents page = do
  frgn <- Toppokki.unsafeEvaluateStringFunction collectEventsJS page
  let
    (encodedEvents :: Array String) = unsafeFromForeign frgn
  -- liftEffect $ Console.log $ "getBrowserEvenrs: " <> show encodedEvents
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
