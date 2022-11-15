module Ctl.Internal.Test.E2E.Browser
  ( withBrowser
  ) where

import Prelude

import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Test.E2E.Types (E2ETestRuntime, ExtensionId, unExtensionId)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff, bracket)
import Toppokki as Toppokki

withBrowser
  :: forall (a :: Type)
   . Boolean
  -> E2ETestRuntime
  -> Maybe ExtensionId
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser noHeadless opts Nothing =
  bracket (launch noHeadless opts) Toppokki.close
withBrowser noHeadless opts (Just extensionId) = bracket
  (launchWithExtension noHeadless opts extensionId)
  Toppokki.close

launch
  :: Boolean
  -> E2ETestRuntime
  -> Aff Toppokki.Browser
launch noHeadless { browser, chromeUserDataDir } =
  do
    Toppokki.launch
      { args
      , headless: not noHeadless
      , userDataDir: chromeUserDataDir
      , executablePath: browser
      }
  where
  args =
    [ "--user-data-dir=" <> chromeUserDataDir
    ] <> if not noHeadless then [ "--headless=chrome" ] else []

launchWithExtension
  :: Boolean
  -> E2ETestRuntime
  -> ExtensionId
  -> Aff Toppokki.Browser
launchWithExtension noHeadless rt@{ browser, chromeUserDataDir } extensionId =
  do
    Toppokki.launch
      { args
      , headless: not noHeadless
      , userDataDir: chromeUserDataDir
      , executablePath: browser
      }
  where
  args =
    [ "--disable-extensions-except=" <> extensionsList
    , "--load-extension=" <> extensionsList
    , "--user-data-dir=" <> chromeUserDataDir
    ] <> if not noHeadless then [ "--headless=chrome" ] else []

  extensionsList :: String
  extensionsList = rt.tmpDir <</>> unExtensionId extensionId
