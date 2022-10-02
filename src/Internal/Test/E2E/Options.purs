module Ctl.Internal.Test.E2E.Options where

import Prelude

import Control.Alt ((<|>))
import Ctl.Internal.Test.E2E.Types
  ( Browser
  , ChromeUserDataDir
  , CrxFilePath
  , E2ETest
  , ExtensionId
  , SettingsArchive
  , TmpDir
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  , WalletPassword
  , mkE2ETest
  , mkExtensionId
  )
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Path (FilePath)
import Options.Applicative
  ( Parser
  , ReadM
  , command
  , execParser
  , fullDesc
  , help
  , helper
  , info
  , int
  , long
  , many
  , metavar
  , option
  , progDesc
  , showDefaultWith
  , str
  , subparser
  , switch
  , value
  , (<**>)
  )
import Options.Applicative.Builder (eitherReader)
import Record.Builder (build, merge)
import Type.Row (type (+))

-- | CLI options for E2E tests.
type TestOptions = Record
  (NoHeadless_ + Tests_ + TestTimeout_ + CommonOptions_ + ())

type NoHeadless_ r = (noHeadless :: Boolean | r)

type Tests_ r = (tests :: Array E2ETest | r)

type TestTimeout_ r = (testTimeout :: Maybe Int | r)

-- | CLI options for both E2E tests and the `browser` command
type CommonOptions_ (r :: Row Type) =
  ( browser :: Maybe Browser
  , wallets :: Map WalletExt ExtensionOptions
  , chromeUserDataDir :: Maybe ChromeUserDataDir
  , tmpDir :: Maybe TmpDir
  , settingsArchive :: Maybe SettingsArchive
  )

-- | CLI options for the `browser` command.
type BrowserOptions = Record (CommonOptions_ ())

-- | Wallet extension options. Everything is wrapped in maybe, because we want
-- | to be able to override every single environment variable via the CLI.
type ExtensionOptions =
  { crxFile :: Maybe CrxFilePath
  , password :: Maybe WalletPassword
  , extensionId :: Maybe ExtensionId
  }

-- | CLI options for `pack` and `unpack` commands.
type SettingsOptions =
  { chromeUserDataDir :: Maybe FilePath
  , settingsArchive :: Maybe FilePath
  }

-- | A CLI command that can be interpreted by the E2E test suite.
data E2ECommand
  = RunE2ETests TestOptions
  | RunBrowser BrowserOptions
  | PackSettings SettingsOptions
  | UnpackSettings SettingsOptions

derive instance Generic E2ECommand _

instance Show E2ECommand where
  show = genericShow

-- | Parse CLI arguments to get E2ECommand that can be run by `runE2E`
parseCliArgs :: Effect E2ECommand
parseCliArgs = execParser $ info (commands <**> helper)
  (progDesc "CTL end-to-end test runner")

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

extensionIdParser :: ReadM ExtensionId
extensionIdParser = eitherReader \s ->
  note
    ( "Unable to parse extension ID. must be a string consisting of 32 \
      \characters, got: " <> s
    )
    $ mkExtensionId s

browserOptionsParser :: Parser BrowserOptions
browserOptionsParser = ado
  browser <- option (Just <$> str) $ fold
    [ long "browser"
    , metavar "FILE"
    , help "Chrome/-ium exe (search in env if not set)"
    , showDefaultWith case _ of
        Nothing -> "E2E_BROWSER env variable value"
        Just a -> show a
    , value Nothing
    ]
  { chromeUserDataDir, settingsArchive } <- settingsOptionsParser
  tmpDir <- option (Just <$> str) $ fold
    [ long "tmp-dir"
    , help "Temporary data directory"
    , value Nothing
    , showDefaultWith case _ of
        Nothing -> "None"
        Just a -> show a
    , metavar "DIR"
    ]
  -- Eternl
  eternlExtId <- option (Just <$> extensionIdParser) $ fold
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
  namiExtId <- option (Just <$> extensionIdParser) $ fold
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
  geroExtId <- option (Just <$> extensionIdParser) $ fold
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
  flintExtId <- option (Just <$> extensionIdParser) $ fold
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
  lodeExtId <- option (Just <$> extensionIdParser) $ fold
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
    { browser, wallets, chromeUserDataDir, tmpDir, settingsArchive }
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
  tests <- testUrlsOptionParser
  noHeadless <- switch $ fold
    [ long "no-headless"
    , help "Show visible browser window"
    ]
  testTimeout <- option (Just <$> int) $ fold
    [ long "test-timeout"
    , help "Timeout for each test"
    , value Nothing
    , showDefaultWith $ const "E2E_TEST_TIMEOUT"
    , metavar "SECONDS"
    ]
  in build (merge res) { noHeadless, tests, testTimeout }

testParser :: ReadM E2ETest
testParser = eitherReader \str ->
  note
    ( "Unable to parse test specification from: " <> str
    )
    $ mkE2ETest str

testUrlsOptionParser :: Parser (Array E2ETest)
testUrlsOptionParser =
  let
    defaultValue = Nil
    listStrOption =
      many $ option testParser (long "test" <> help helpText)
  in
    Array.fromFoldable <$> (listStrOption <|> (pure defaultValue))
  where
  helpText =
    "Specification of a test. Consists of a wallet name and a URL, separated \
    \by `:`. Can be specified multiple times. Default: empty"

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
