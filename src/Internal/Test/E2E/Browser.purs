module Ctl.Internal.Test.E2E.Browser
  ( withBrowser
  ) where

import Prelude

import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Test.E2E.Types
  ( BrowserArg
  , E2ETestRuntime
  , ExtensionId
  , unExtensionId
  )
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff, bracket)
import Toppokki as Toppokki

withBrowser
  :: forall (a :: Type)
   . Boolean
  -> Array BrowserArg
  -> E2ETestRuntime
  -> Maybe ExtensionId
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser noHeadless extraArgs opts Nothing =
  bracket (launch noHeadless extraArgs opts) Toppokki.close
withBrowser noHeadless extraArgs opts (Just extensionId) = bracket
  (launchWithExtension noHeadless extraArgs opts extensionId)
  Toppokki.close

launch
  :: Boolean
  -> Array BrowserArg
  -> E2ETestRuntime
  -> Aff Toppokki.Browser
launch noHeadless extraBrowserArgs { browser, chromeUserDataDir } =
  do
    Toppokki.launch
      { args
      , headless: not noHeadless
      , userDataDir: chromeUserDataDir
      , executablePath: browser
      }
  where
  args =
    (if not noHeadless then [ "--headless=chrome" ] else []) <>
      extraBrowserArgs

launchWithExtension
  :: Boolean
  -> Array BrowserArg
  -> E2ETestRuntime
  -> ExtensionId
  -> Aff Toppokki.Browser
launchWithExtension
  noHeadless
  extraBrowserArgs
  rt@{ browser, chromeUserDataDir }
  extensionId =
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
    ] <> (if not noHeadless then [ "--headless=chrome" ] else [])
      <> extraBrowserArgs

  extensionsList :: String
  extensionsList = rt.tmpDir <</>> unExtensionId extensionId
