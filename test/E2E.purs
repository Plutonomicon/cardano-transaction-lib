module Test.E2E (main) where

import Control.Applicative (apply, (<$>))
import Data.Foldable (fold)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Mote (group)
import Test.Utils as Utils
import TestM (TestPlanM)
import Prelude (Unit, ($), map, bind, pure)
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Test.E2E.Browser (Mode(Headless, Visible), launchWithNami)
import Test.Spec.Runner as SpecRunner
import Options.Applicative
import Toppokki as Toki

data TestOptions = TestOptions
  { chromeExe :: Maybe String
  , namiDir :: String
  , chromeUserDataDir :: String
  , noHeadless :: Boolean
  , dumpIO :: Boolean
  }

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
  dumpIO <- switch $ fold
    [ long "dumpio"
    , help "Print browser console output to terminal"
    ]
  in TestOptions { chromeExe, namiDir, chromeUserDataDir, noHeadless, dumpIO }

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = do
  TestOptions
    { namiDir
    , noHeadless
    , chromeExe
    , chromeUserDataDir
    , dumpIO
    } <- execParser $ info optParser fullDesc
  launchAff_ $ do
    browser <- launchWithNami chromeExe chromeUserDataDir namiDir
      (headlessMode noHeadless)
      dumpIO
    Utils.interpret'
      (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
      (testPlan browser)
  where
  headlessMode :: Boolean -> Mode
  headlessMode noHeadless =
    if noHeadless then Visible
    else Headless

-- Requires external services listed in README.md
testPlan :: Toki.Browser -> TestPlanM Unit
testPlan browser = group "e2e tests" do
  testPkh2Pkh browser

