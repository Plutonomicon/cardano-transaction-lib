-- | A module for communication between E2E test suite and a headless browser.
-- | This module exposes API for the NodeJS side.
-- | See `Ctl.Internal.Test.E2E.Feedback.Browser` for the corresponding APIs
-- | for the NodeJS side.
module Ctl.Internal.Test.E2E.Feedback.Node
  ( setClusterSetup
  , subscribeToBrowserEvents
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Control.Lazy (fix)
import Ctl.Internal.Helpers (liftEither, race)
import Ctl.Internal.QueryM (ClusterSetup)
import Ctl.Internal.Test.E2E.Feedback (BrowserEvent)
import Data.Either (hush, note)
import Data.Traversable (for, traverse_)
import Effect.AVar as AVarSync
import Effect.Aff
  ( Aff
  , Milliseconds(Milliseconds)
  , delay
  , launchAff_
  , throwError
  )
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import Foreign (unsafeFromForeign)
import Toppokki as Toppokki

-- | React to events raised by the browser
-- |
-- | Takes a page and a function which provides you with
-- | a `wait :: Number -> String -> Aff BrowserEvent` function.
-- | `wait` takes timeout in seconds, timeout error message and
-- | returns `Aff` action, wich, when performed, produce you the next `BrowserEvent`
subscribeToBrowserEvents
  :: Toppokki.Page
  -> ((Number -> String -> Aff BrowserEvent) -> Aff Unit)
  -> Aff Unit
subscribeToBrowserEvents page cont = do
  logs <- liftEffect $ Ref.new ""
  eventAVar <- AVar.empty

  let
    addLogLine line = Ref.modify_ (flip append (line <> "\n")) logs
    delayS s = delay $ Milliseconds (1000.0 * s)

  liftEffect do
    flip Toppokki.onConsole page $
      mkEffectFn1 \cm -> launchAff_ do
        Toppokki.consoleMessageText cm >>= liftEffect <<< addLogLine

    flip Toppokki.onPageError page $
      mkEffectFn1 \err -> do
        allLogs <- Ref.read logs
        Console.log allLogs

        let errStr = "Page error occured: " <> message err <> "\n" <> allLogs
        AVarSync.kill (error errStr) eventAVar

  let
    -- Asyncronously gets browser events and pushes
    -- to the `eventVAar` one by one.
    watcher = fix \this -> do
      getBrowserEvents page >>= traverse_ (_ `AVar.put` eventAVar)
      delayS 0.5 *> this

    handler = cont \timeoutS timeoutError ->
      race
        -- "eventAVar consumer site"
        -- Either provides next `BrowserEvent`, or throws an exception if
        -- `eventAVar` was killed in `onPageError`` callback
        (AVar.take eventAVar) $
        -- Watchdog timer. Will be canceled (by race)
        -- when `AVar.take eventAVar` completes
        -- If watchdog is not canceled in `timeuotS` seconds it
        -- resolves race with an excetion
        delayS timeoutS *> throwError (error timeoutError)

  -- Watcher loop, normally, runs forever
  -- Gets resolved by handler or if watcher throws an error
  race watcher handler

getBrowserEvents
  :: Toppokki.Page -> Aff (Array BrowserEvent)
getBrowserEvents page = do
  frgn <- Toppokki.unsafeEvaluateStringFunction collectEventsJS page
  let
    (encodedEvents :: Array String) = unsafeFromForeign frgn
  for encodedEvents \event -> do
    liftEither $ note (error $ "Unable to decode BrowserEvent from: " <> event)
      $ hush
      $ decodeAeson =<< parseJsonStringToAeson event
  where
  collectEventsJS :: String
  collectEventsJS =
    """
      (() => {
        const res = window.ctlE2ECommunications || [];
        window.ctlE2ECommunications = [];
        return res;
      })()
      """

-- | Injects cluster setup value into the JS context of the page, where it can
-- | be retrieved using `getClusterSetup`.
setClusterSetup :: Toppokki.Page -> ClusterSetup -> Aff Unit
setClusterSetup page clusterSetup = do
  let
    jsCode = "(() => window.ctlE2EClusterSetup = ("
      <> stringifyAeson (encodeAeson clusterSetup)
      <> "))()"
  void $ Toppokki.unsafeEvaluateStringFunction jsCode page
