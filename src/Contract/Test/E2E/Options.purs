module Contract.Test.E2E.Options where

import Prelude

import Contract.Test.E2E.Helpers (WalletPassword(WalletPassword))
import Contract.Test.E2E.WalletExt
  ( WalletConfig(WalletConfig)
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  )
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Node.Path (FilePath)
import Options.Applicative
  ( Parser
  , command
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , showDefaultWith
  , str
  , strOption
  , subparser
  , switch
  , value
  , (<**>)
  )
import Options.Applicative.Types (optional)
import Prim.Row as Row
import Record.Builder (build, merge)
import Toppokki as Toppokki
import Type.Row (type (+))
import Undefined (undefined)

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
-- TODO: rename to E2ETestOptions
type TestOptions = Record (NoHeadless_ + MainOptions_ + ())

type NoHeadless_ r = (noHeadless :: Boolean | r)

type MainOptions_ (r :: Row Type) =
  ( chromeExe :: Maybe FilePath
  , wallets :: Map WalletExt WalletConfig
  , chromeUserDataDir :: Maybe FilePath
  , tempDir :: Maybe FilePath
  )

type BrowserOptions = Record (MainOptions_ ())

data Mode = Headless | Visible

derive instance Eq Mode

type SettingsOptions =
  { chromeUserDataDir :: Maybe FilePath
  , settingsArchive :: Maybe FilePath
  }

data E2ECommand
  = RunE2ETests TestOptions
  | RunBrowser BrowserOptions
  | PackSettings SettingsOptions
  | UnpackSettings SettingsOptions

parser :: Parser E2ECommand
parser = subparser $
  command "run"
    ( info (RunE2ETests <$> testOptionsParser <**> helper)
        (progDesc "Run the tests")
    )
    <> command "browser"
      ( info (RunBrowser <$> browserOptionsParser <**> helper)
          (progDesc "Run the browser")
      )
    <> command "pack"
      ( info (PackSettings <$> settingsOptionsParser <**> helper)
          (progDesc "Save settings to the archive")
      )
    <> command "unpack"
      ( info (UnpackSettings <$> settingsOptionsParser <**> helper)
          (progDesc "Load settings from the archive")
      )

browserOptionsParser :: Parser BrowserOptions
browserOptionsParser = ado
  chromeExe <- option (Just <$> str) $ fold
    [ long "chrome-exe"
    , metavar "FILE"
    , help "Chrome/-ium exe (search in env if not set)"
    , value Nothing
    ]
  eternlDir <- option (Just <$> str) $ fold
    [ long "eternl-dir"
    , metavar "DIR"
    , help "Directory where Eternl is unpacked"
    , value Nothing
    ]
  eternlPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "eternl-password"
    , metavar "PW"
    , help "Eternl wallet password"
    , value Nothing
    ]
  namiDir <- option (Just <$> str) $ fold
    [ long "nami-dir"
    , metavar "DIR"
    , help "Directory where nami is unpacked"
    , value Nothing
    ]
  namiPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "nami-password"
    , metavar "PW"
    , help "Nami wallet password"
    , value Nothing
    ]
  geroDir <- option (Just <$> str) $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  geroPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "gero-password"
    , metavar "PW"
    , help "Gero wallet password"
    , value Nothing
    ]
  flintDir <- option (Just <$> str) $ fold
    [ long "flint-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  flintPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "flint-password"
    , metavar "PW"
    , help "Flint wallet password"
    , value Nothing
    ]
  lodeDir <- option (Just <$> str) $ fold
    [ long "lode-dir"
    , metavar "DIR"
    , help "Directory where lode is unpacked"
    , value Nothing
    ]
  lodePassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "lode-password"
    , metavar "PW"
    , help "Lode wallet password"
    , value Nothing
    ]
  chromeUserDataDir <- optional $ option str $ fold
    [ long "chrome-user-data"
    , help "Chrome/-ium user data dir"
    , value "test-data/chrome-user-data"
    , showDefault
    , metavar "DIR"
    ]
  tempDir <- option (Just <$> str) $ fold
    [ long "tmp-dir"
    , help "Temporary data directory"
    , value Nothing
    , showDefaultWith case _ of
        Nothing -> "None"
        Just a -> show a
    , metavar "DIR"
    ]
  browserPath <- option (Just <$> str) $ fold
    [ long "browser"
    , help "Browser binary to use"
    , value Nothing
    , showDefaultWith case _ of
        Nothing -> "E2E_BROWSER env variable value"
        Just a -> show a
    , metavar "BINARY"
    ]
  let
    wallets = Map.fromFoldable $ catMaybes
      [ mkConfig NamiExt namiDir namiPassword
      , mkConfig GeroExt geroDir geroPassword
      , mkConfig FlintExt flintDir flintPassword
      , mkConfig LodeExt lodeDir lodePassword
      , mkConfig EternlExt eternlDir eternlPassword
      ]
  in
    { chromeExe, wallets, chromeUserDataDir, tempDir }
  where
  mkConfig
    :: WalletExt
    -> Maybe FilePath
    -> Maybe WalletPassword
    -> Maybe (Tuple WalletExt WalletConfig)
  mkConfig ext mfp mpw = map (ext /\ _) $ WalletConfig <$> mfp <*> mpw

testOptionsParser :: Parser TestOptions
testOptionsParser = ado
  res <- browserOptionsParser
  noHeadless <- switch $ fold
    [ long "no-headless"
    , help "Show visible browser window"
    ]
  in build (merge res) { noHeadless }

chromeUserDataOption :: Parser (Maybe String)
chromeUserDataOption = option (Just <$> str) $ fold
  [ long "chrome-user-data"
  , help "Chrome/-ium user data dir"
  , value Nothing
  , showDefaultWith $ const "E2E_CHROME_USER_DATA"
  , metavar "DIR"
  ]

settingsOptionsParser :: Parser SettingsOptions
settingsOptionsParser = ado
  chromeUserDataDir <- chromeUserDataOption
  settingsArchive <- option (Just <$> str) $ fold
    [ long "settings-archive"
    , help "Settings archive (.tar.gz) that will be used to store the settings"
    , value Nothing
    , showDefaultWith show
    , metavar "DIR"
    ]
  in { chromeUserDataDir, settingsArchive }

parseOptions :: Effect TestOptions
parseOptions = execParser $ info testOptionsParser fullDesc
