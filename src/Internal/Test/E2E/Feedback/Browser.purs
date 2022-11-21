-- | A module for communication between E2E test suite and a headless browser.
-- | This module exposes API for the browser side.
-- | See `Ctl.Internal.Test.E2E.Feedback.Node` for the corresponding APIs for
-- | the NodeJS side.
module Ctl.Internal.Test.E2E.Feedback.Browser
  ( pushBrowserEvent
  , getClusterSetup
  , getClusterSetupRepeatedly
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, jsonToAeson, stringifyAeson)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.QueryM (ClusterSetup)
import Ctl.Internal.Test.E2E.Feedback (BrowserEvent)
import Data.Argonaut (Json)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds(Seconds))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Retry (constantDelay, limitRetries, recovering)
import Effect.Class (liftEffect)
import Effect.Exception (error)

pushBrowserEvent
  :: BrowserEvent -> Effect Unit
pushBrowserEvent ev = do
  _pushBrowserEvent <<< stringifyAeson $ encodeAeson ev

foreign import _pushBrowserEvent :: String -> Effect Unit

-- | Attempts to read cluster setup multiple times
getClusterSetupRepeatedly :: Aff ClusterSetup
getClusterSetupRepeatedly = do
  recovering
    (constantDelay (Seconds 0.5) <> limitRetries 10)
    [ \_ _ -> pure true ]
    \_ -> do
      liftEffect $ liftedM (error errorMessage) getClusterSetup
  where
  errorMessage =
    "Unable to get local cluster setup. Common reasons are:\n\
    \ - The E2E contract is not served properly\n\
    \ - CIP-30 mock is used, but signing keys have not been provided in the\
    \ test URL."

-- | Retrieves cluster setup provided by the NodeJS side of the test suite.
getClusterSetup :: Effect (Maybe ClusterSetup)
getClusterSetup = do
  _getClusterSetup maybeFfiHelper <#>
    (_ >>= jsonToAeson >>> decodeAeson >>> hush)

foreign import _getClusterSetup :: MaybeFfiHelper -> Effect (Maybe Json)
