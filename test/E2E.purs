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
import Prelude (Unit, ($), map, bind, discard, pure)
import Test.E2E.Browser (Mode(Headless, Visible), launchWithExtension)
import Test.Spec.Runner as SpecRunner
import Options.Applicative
import Toppokki as Toki
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Test.Examples.Gero (testGero)

data TestOptions = TestOptions
  { chromeExe :: Maybe String
  , namiDir :: String
  , geroDir :: String
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
  dumpIO <- switch $ fold
    [ long "dumpio"
    , help "Print browser console output to terminal"
    ]
  in
    TestOptions
      { chromeExe, namiDir, geroDir, chromeUserDataDir, noHeadless, dumpIO }

-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = do
  TestOptions
    { namiDir
    , geroDir
    , noHeadless
    , chromeExe
    , chromeUserDataDir
    , dumpIO
    } <- execParser $ info optParser fullDesc
  launchAff_ $ do
    {-    browserWithNami <- launchWithExtension chromeExe chromeUserDataDir namiDir
    (headlessMode noHeadless)
    dumpIO -}
    browserWithGero <- launchWithExtension chromeExe chromeUserDataDir geroDir
      (headlessMode noHeadless)
      dumpIO
    Utils.interpret'
      (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
      (testPlan browserWithGero browserWithGero)
  where
  headlessMode :: Boolean -> Mode
  headlessMode noHeadless =
    if noHeadless then Visible
    else Headless

-- Requires external services listed in README.md
testPlan :: Toki.Browser -> Toki.Browser -> TestPlanM Unit
testPlan browserWithNami browserWithGero = group "e2e tests" do
  testGero browserWithGero
--  testPkh2Pkh browserWithNami

