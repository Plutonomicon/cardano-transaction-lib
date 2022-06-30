module Test.E2E.Wallet where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (typeOf)
import Mote (group, test)
import TestM (TestPlanM)
import Toppokki as Toki
import Test.Examples.Config (userDataDir)
import Test.Spec.Assertions (shouldEqual)

namiHash :: String
namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"

namiPath :: String
namiPath = "/home/mike/.config/google-chrome/Default/Extensions/" <> namiHash <>
  "/3.2.5_1"

data Mode = Headless | Visible

derive instance Eq Mode

chromeArgsWithNami :: Mode -> Array String
chromeArgsWithNami mode =
  [ "--disable-extensions-except=" <> namiPath
  , "--load-extension=" <> namiPath
  ] <> if mode == Headless then [ "--headless=chrome" ] else []

launchWithNami :: Mode -> Aff Toki.Browser
launchWithNami mode =
  Toki.launch
    { args: chromeArgsWithNami mode
    , headless: mode == Headless
    , userDataDir: userDataDir
    }

launchWithNami' :: Aff Toki.Browser
launchWithNami' = launchWithNami Headless

