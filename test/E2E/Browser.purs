module Test.E2E.Browser
  ( Mode(Headless, Visible)
  , TestOptions(TestOptions)
  , WalletExt (GeroExt, NamiExt)
  , withBrowser
  , parseOptions
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Node.Path (FilePath)
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

data WalletExt = NamiExt | GeroExt

-- | Parameters for E2E tests
-- | 'chromeExe' should point to a chromium or google-chrome binary.
-- | 'namiDir' should point to a directory where Nami is unpacked.
-- | 'geroDir' should point to a directory where Gero is unpacked.
-- |    both Nami and Gero must be provided to run the full suite of tests.
-- | 'chromeUserDataDir' should point to a chromium profile directory. If
-- |    not provided, test-data/chrome-user-data is used.
-- |    'make e2e-test' then takes care to unpack Nami and Gero settings to there,
-- |    so that wallet data is available.
-- | 'noHeadless' is a flag to display the browser during the tests.
data TestOptions = TestOptions
  { chromeExe :: Maybe FilePath
  , namiDir :: FilePath
  , geroDir :: FilePath
  , chromeUserDataDir :: FilePath
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

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> WalletExt
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension ext opts) Toppokki.close

launchWithExtension
  :: WalletExt -> TestOptions -> Aff Toppokki.Browser
launchWithExtension walletExt
  (TestOptions { chromeExe, chromeUserDataDir, namiDir, geroDir, noHeadless }) =
    Toppokki.launch
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
  mode
    | noHeadless = Visible 
    | otherwise = Headless

  extDir :: FilePath
  extDir = case walletExt of
    GeroExt -> geroDir
    NamiExt -> namiDir
