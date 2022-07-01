module Test.E2E.Wallet
  ( namiHash
  , Mode(..)
  , launchWithNami
  , launchWithNami'
  ) where

import Prelude

import Effect.Aff (Aff)
import Toppokki as Toki
import Test.Examples.Config (userDataDir)

namiHash :: String
namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"

data Mode = Headless | Visible

derive instance Eq Mode

chromeArgsWithNami :: String -> Mode -> Array String
chromeArgsWithNami namiPath mode =
  [ "--disable-extensions-except=" <> namiPath
  , "--load-extension=" <> namiPath
  ] <> if mode == Headless then [ "--headless=chrome" ] else []

launchWithNami :: String -> Mode -> Aff Toki.Browser
launchWithNami namiDir mode =
  Toki.launch
    { args: chromeArgsWithNami namiDir mode
    , headless: mode == Headless
    , userDataDir: userDataDir
    }

launchWithNami' :: String -> Aff Toki.Browser
launchWithNami' namiDir = launchWithNami namiDir Headless

