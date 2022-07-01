module Test.E2E (main) where

import Control.Applicative (apply, (<$>))
import Data.Foldable (fold)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Utils as Utils
import TestM (TestPlanM)
import Prelude (Unit, ($), (>>=), pure, map)
import Test.Examples.Pkh2Pkh (testPkh2Pkh)
import Test.E2E.Wallet (Mode(Headless, Visible))
import Test.Spec.Runner as SpecRunner
import Options.Applicative

data TestOptions = TestOptions
                   { chromeExe         :: Maybe String
                   , namiDir           :: String
                   , chromeUserDataDir :: String
                   , noHeadless        :: Boolean
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
  in TestOptions { chromeExe, namiDir, chromeUserDataDir, noHeadless }
  
-- Run with `spago test --main Test.E2E`
main :: Effect Unit
main = execParser opts >>= \testOpts -> launchAff_ $
  Utils.interpret'
    (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    (testPlan testOpts)
  where
    opts = info optParser fullDesc
                                      
-- Requires external services listed in README.md
testPlan :: TestOptions -> TestPlanM Unit
testPlan (TestOptions { namiDir, noHeadless }) =
  let mode = if noHeadless
             then Visible
             else Headless
  in testPkh2Pkh namiDir mode

