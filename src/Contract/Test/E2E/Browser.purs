module Contract.Test.E2E.Browser
  ( withBrowser
  , module X
  ) where

import Prelude

import Contract.Test.E2E.Options (Mode(Headless, Visible))
import Contract.Test.E2E.Options
  ( Mode(Headless, Visible)
  , TestOptions
  , parseOptions
  ) as X
import Contract.Test.E2E.Types (E2ETestRuntime, ExtensionId)
import Effect.Aff (Aff, bracket)
import Toppokki as Toppokki

withBrowser
  :: forall (a :: Type)
   . Boolean
  -> E2ETestRuntime
  -> ExtensionId
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser noHeadless opts extensionId = bracket
  (launchWithExtension noHeadless opts extensionId)
  (Toppokki.close)

launchWithExtension
  :: Boolean
  -> E2ETestRuntime
  -> ExtensionId
  -> Aff Toppokki.Browser
launchWithExtension noHeadless rt@{ browserPath, chromeUserDataDir } extensionId =
  do
    Toppokki.launch
      { args
      , headless: mode == Headless
      , userDataDir: chromeUserDataDir
      , executablePath: browserPath
      }
  where
  args =
    [ "--disable-extensions-except=" <> extensionsList
    , "--load-extension=" <> extensionsList
    , "--user-data-dir=" <> chromeUserDataDir
    ] <> if mode == Headless then [ "--headless=chrome" ] else []

  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless

  extensionsList :: String
  extensionsList = rt.tempDir <> "/" <> extensionId
