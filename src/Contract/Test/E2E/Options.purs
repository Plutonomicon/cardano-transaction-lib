module Contract.Test.E2E.Options where

import Prelude

import Contract.Test.E2E.Helpers (WalletPassword)
import Contract.Test.E2E.Types
  ( CrxFilePath
  , ExtensionId
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  )
import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
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
  , many
  , metavar
  , option
  , progDesc
  , showDefaultWith
  , str
  , strOption
  , subparser
  , switch
  , value
  , (<**>)
  )
import Record.Builder (build, merge)
import Type.Row (type (+))

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
type TestOptions = Record (NoHeadless_ + TestUrlsOptions_ + MainOptions_ + ())

type NoHeadless_ r = (noHeadless :: Boolean | r)

type TestUrlsOptions_ r = (testUrls :: Array String | r)

type ExtensionOptions =
  { crxFile :: Maybe String
  , password :: Maybe WalletPassword
  , extensionId :: Maybe ExtensionId
  }

type MainOptions_ (r :: Row Type) =
  ( chromeExe :: Maybe FilePath
  , wallets :: Map WalletExt ExtensionOptions
  , chromeUserDataDir :: Maybe FilePath
  , tempDir :: Maybe FilePath
  , settingsArchive :: Maybe FilePath
  )

type BrowserOptions = Record (MainOptions_ ())

data Mode = Headless | Visible

derive instance Eq Mode

type SettingsOptions =
  { chromeUserDataDir :: Maybe FilePath
  , settingsArchive :: Maybe FilePath
  }

-- | A CLI command that can be interpreted by E2E test suite.
data E2ECommand
  = RunE2ETests TestOptions
  | RunBrowser BrowserOptions
  | PackSettings SettingsOptions
  | UnpackSettings SettingsOptions

derive instance Generic E2ECommand _

instance Show E2ECommand where
  show = genericShow

parseCommand :: Effect E2ECommand
parseCommand = execParser $ info (commands <**> helper)
  (progDesc "E2E test runner")

commands :: Parser E2ECommand
commands = subparser $
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
    , showDefaultWith case _ of
        Nothing -> "E2E_BROWSER env variable value"
        Just a -> show a
    , value Nothing
    ]
  { chromeUserDataDir, settingsArchive } <- settingsOptionsParser
  tempDir <- option (Just <$> str) $ fold
    [ long "tmp-dir"
    , help "Temporary data directory"
    , value Nothing
    , showDefaultWith case _ of
        Nothing -> "None"
        Just a -> show a
    , metavar "DIR"
    ]
  -- Eternl
  eternlExtId <- option (Just <$> str) $ fold
    [ long "eternl-extid"
    , metavar "EXTID"
    , help "Eternl extension ID"
    , value Nothing
    ]
  eternlPassword <- option (Just <$> str) $ fold
    [ long "eternl-password"
    , metavar "PASSWORD"
    , help "Eternl wallet password"
    , value Nothing
    ]
  eternlCrx <- option (Just <$> str) $ fold
    [ long "eternl-crx"
    , metavar "FILE"
    , help "Eternl wallet extension (.crx) file"
    , value Nothing
    ]
  -- Nami
  namiExtId <- option (Just <$> str) $ fold
    [ long "nami-extid"
    , metavar "EXTID"
    , help "Nami extension ID"
    , value Nothing
    ]
  namiPassword <- option (Just <$> str) $ fold
    [ long "nami-password"
    , metavar "PASSWORD"
    , help "Nami wallet password"
    , value Nothing
    ]
  namiCrx <- option (Just <$> str) $ fold
    [ long "nami-crx"
    , metavar "FILE"
    , help "Nami wallet extension (.crx) file"
    , value Nothing
    ]
  -- Gero
  geroExtId <- option (Just <$> str) $ fold
    [ long "gero-extid"
    , metavar "EXTID"
    , help "Gero extension ID"
    , value Nothing
    ]
  geroPassword <- option (Just <$> str) $ fold
    [ long "gero-password"
    , metavar "PASSWORD"
    , help "Gero wallet password"
    , value Nothing
    ]
  geroCrx <- option (Just <$> str) $ fold
    [ long "greo-crx"
    , metavar "FILE"
    , help "Gero wallet extension (.crx) file"
    , value Nothing
    ]
  -- Flint
  flintExtId <- option (Just <$> str) $ fold
    [ long "flint-extid"
    , metavar "EXTID"
    , help "Flint extension ID"
    , value Nothing
    ]
  flintPassword <- option (Just <$> str) $ fold
    [ long "flint-password"
    , metavar "PASSWORD"
    , help "Flint wallet password"
    , value Nothing
    ]
  flintCrx <- option (Just <$> str) $ fold
    [ long "flint-crx"
    , metavar "FILE"
    , help "Flint wallet extension (.crx) file"
    , value Nothing
    ]
  -- Lode
  lodeExtId <- option (Just <$> str) $ fold
    [ long "lode-extid"
    , metavar "EXTID"
    , help "Lode extension ID"
    , value Nothing
    ]
  lodePassword <- option (Just <$> str) $ fold
    [ long "lode-password"
    , metavar "PASSWORD"
    , help "Lode wallet password"
    , value Nothing
    ]
  lodeCrx <- option (Just <$> str) $ fold
    [ long "lode-crx"
    , metavar "FILE"
    , help "Lode wallet extension (.crx) file"
    , value Nothing
    ]
  let
    wallets = Map.fromFoldable $ catMaybes
      [ mkConfig NamiExt namiExtId namiPassword namiCrx
      , mkConfig GeroExt geroExtId geroPassword geroCrx
      , mkConfig FlintExt flintExtId flintPassword flintCrx
      , mkConfig LodeExt lodeExtId lodePassword lodeCrx
      , mkConfig EternlExt eternlExtId eternlPassword eternlCrx
      ]
  in
    { chromeExe, wallets, chromeUserDataDir, tempDir, settingsArchive }
  where
  mkConfig
    :: WalletExt
    -> Maybe ExtensionId
    -> Maybe WalletPassword
    -> Maybe CrxFilePath
    -> Maybe (WalletExt /\ ExtensionOptions)
  mkConfig _ Nothing Nothing Nothing = Nothing
  mkConfig ext extensionId password crxFile =
    Just $ ext /\
      { crxFile
      , password: password
      , extensionId
      }

testOptionsParser :: Parser TestOptions
testOptionsParser = ado
  res <- browserOptionsParser
  testUrls <- testUrlsOptionParser
  noHeadless <- switch $ fold
    [ long "no-headless"
    , help "Show visible browser window"
    ]
  in build (merge res) { noHeadless, testUrls }

testUrlsOptionParser :: Parser (Array String)
testUrlsOptionParser =
  let
    defaultValue = Nil
    listStrOption =
      many $ strOption $ long "test-url" <> help helpText
  in
    Array.fromFoldable <$> (listStrOption <|> (pure defaultValue))
  where
  helpText =
    "URL of a hosted test example. Can be specified multiple times. Default: []"

chromeUserDataOptionParser :: Parser (Maybe String)
chromeUserDataOptionParser = option (Just <$> str) $ fold
  [ long "chrome-user-data"
  , help "Chrome/-ium user data dir"
  , value Nothing
  , showDefaultWith $ const "E2E_CHROME_USER_DATA"
  , metavar "DIR"
  ]

settingsOptionsParser :: Parser SettingsOptions
settingsOptionsParser = ado
  chromeUserDataDir <- chromeUserDataOptionParser
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
