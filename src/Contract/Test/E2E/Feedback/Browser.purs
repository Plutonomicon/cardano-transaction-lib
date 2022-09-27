-- | A module for communication between E2E test suite and a headless browser.
-- | This module exposes API for the browser side.
-- | See `Contract.Test.E2E.Feedback.Node` for the corresponding APIs for the
-- | NodeJS side.
module Contract.Test.E2E.Feedback.Browser
  ( pushBrowserEvent
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson, stringifyAeson)
import Contract.Test.E2E.Feedback (BrowserEvent)
import Effect (Effect)
import Effect.Console as Console

pushBrowserEvent
  :: BrowserEvent -> Effect Unit
pushBrowserEvent ev = do
  Console.log $ show $ encodeAeson ev
  _pushBrowserEvent <<< stringifyAeson $ encodeAeson ev

foreign import _pushBrowserEvent :: String -> Effect Unit
