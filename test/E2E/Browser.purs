module Test.E2E.Browser
  ( Mode(..)
  , launchWithExtension
  ) where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Toppokki as Toki

data Mode = Headless | Visible

derive instance Eq Mode

launchWithExtension
  :: Maybe String -> String -> String -> Mode -> Boolean -> Aff Toki.Browser
launchWithExtension chromeExe chromeUserDataDir extDir mode dumpIO =
  Toki.launch
    { args:
        [ "--disable-extensions-except=" <> extDir
        , "--load-extension=" <> extDir
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    , dumpio: dumpIO
    , devtools: dumpIO
    }

