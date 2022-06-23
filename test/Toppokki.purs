module Test.Toppoki where

import Toppokki as Toki
import Prelude

namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"
namiPath = "/home/mike/.config/google-chrome/Default/Extensions/" <> namiHash <>
  "/3.2.5_1"

welcomePage = "chrome-extension://" <> namiHash <> "/index.html"
otherPage = "chrome-extension://" <> namiHash <> "/mainPopup.bundle.js"
jsPage = "chrome-extension://" <> namiHash <> "/injected.bundle.js"
normalPage = "http://www.orf.at/"
example = "http://localhost:4008/"

--launchOptions :: Toki.LaunchOptions
launchOptions =
  { args:
      [ "--disable-extensions-except=" <> namiPath
      , "--load-extension=" <> namiPath
      --                         , "--headless=chrome"
      ]
  , userDataDir: "/tmp/ChromeProfile"
  }

launchChrome = Toki.launch launchOptions

