module Test.E2E.Browser
  ( namiHash
  , Mode(..)
  , launchWithNami
  ) where

import Prelude
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Toppokki as Toki

namiHash :: String
namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"

data Mode = Headless | Visible

derive instance Eq Mode

launchWithNami
  :: Maybe String -> String -> String -> Mode -> Boolean -> Aff Toki.Browser
launchWithNami chromeExe chromeUserDataDir namiDir mode dumpIO =
  Toki.launch
    { args:
        [ "--disable-extensions-except=" <> namiDir
        , "--load-extension=" <> namiDir
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    , dumpio: dumpIO
    , devtools: dumpIO
    }

