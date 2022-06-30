module Test.Toppokki (launchChrome, exampleUrl) where

import Effect.Aff (Aff)
import Toppokki as Toki
import Prelude

exampleUrl :: String
exampleUrl = "http://localhost:4008/"

launchChrome :: Aff Toki.Browser
launchChrome =
  let
    namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"
    namiPath = "/home/mike/.config/google-chrome/Default/Extensions/"
      <> namiHash
      <> "/3.2.5_1"
  in
    Toki.launch
      { args:
          [ "--disable-extensions-except=" <> namiPath
          , "--load-extension=" <> namiPath
          --                         , "--headless=chrome"
          ]
      , userDataDir: "/tmp/ChromeProfile"
      }

