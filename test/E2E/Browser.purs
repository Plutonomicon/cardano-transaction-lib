module Test.E2E.Browser
  ( Mode(..)
  , TestOptions(..)
  , launchWithExtension
  , parseOptions
  ) where

import Prelude

import Control.Applicative (apply, (<$>))
import Data.Foldable (fold)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Options.Applicative
  ( Parser
  , execParser
  , help
  , long
  , metavar
  , option
  , showDefault
  , str
  , strOption
  , switch
  , value
  , info
  , fullDesc
  )
import Toppokki as Toppokki

data TestOptions = TestOptions
  { chromeExe :: Maybe String
  , namiDir :: String
  , geroDir :: String
  , chromeUserDataDir :: String
  , noHeadless :: Boolean
  }

data Mode = Headless | Visible

derive instance Eq Mode

optParser :: Parser TestOptions
optParser = ado
  chromeExe <- option (Just <$> str) $ fold
    [ long "chrome-exe"
    , metavar "FILE"
    , help "Chrome/-ium exe (search in env if not set)"
    , value Nothing
    ]
  namiDir <- strOption $ fold
    [ long "nami-dir"
    , metavar "DIR"
    , help "Directory where nami is unpacked"
    ]
  geroDir <- strOption $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    ]
  chromeUserDataDir <- strOption $ fold
    [ long "chrome-user-data"
    , help "Chrome/-ium user data dir"
    , value "test-data/chrome-user-data"
    , showDefault
    , metavar "DIR"
    ]
  noHeadless <- switch $ fold
    [ long "no-headless"
    , help "Show visible browser window"
    ]
  in
    TestOptions
      { chromeExe, namiDir, geroDir, chromeUserDataDir, noHeadless }

parseOptions :: Effect TestOptions
parseOptions = execParser $ info optParser fullDesc

launchWithExtension
  :: TestOptions -> String -> Aff Toppokki.Browser
launchWithExtension
  (TestOptions { chromeExe, chromeUserDataDir, namiDir, geroDir, noHeadless })
  wallet = Toppokki.launch
  { args:
      [ "--disable-extensions-except=" <> extDir
      , "--load-extension=" <> extDir
      ] <> if mode == Headless then [ "--headless=chrome" ] else []
  , headless: mode == Headless
  , userDataDir: chromeUserDataDir
  , executablePath: fromMaybe "" chromeExe
  }
  where
  mode :: Mode
  mode = if noHeadless then Visible else Headless

  extDir :: String
  extDir = if wallet == "Gero" then geroDir else namiDir
