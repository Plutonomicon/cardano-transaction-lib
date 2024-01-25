module Ctl.Internal.Test.E2E.Options
  ( TestOptions
  , BrowserOptions
  , ExtensionOptions
  , NoHeadless_
  , Tests_
  , TestTimeout_
  , CommonOptions_
  , ClusterPortsOptions_
  , ClusterPortsOptions
  , SettingsOptions
  , E2ECommand
      ( RunE2ETests
      , RunBrowser
      , PackSettings
      , UnpackSettings
      )
  , parseCliArgs
  , commands
  , parseOptions
  , defaultPorts
  ) where

import Prelude

import Affjax (URL)
import Control.Alt ((<|>))
import Ctl.Internal.Test.E2E.Types
  ( Browser
  , BrowserArg
  , ChromeUserDataDir
  , CrxFilePath
  , E2ETest
  , ExtensionId
  , SettingsArchive
  , SettingsArchiveUrl
  , TmpDir
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  , WalletPassword
  , mkE2ETest
  , mkExtensionId
  )
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (note)
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.String.Utils (words) as String
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
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
  , showDefault
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
  ( NoHeadless_ + Tests_ + TestTimeout_ + ClusterPortsOptions_
      + CommonOptions_
      + ()
  )

type ClusterPortsOptions_ (r :: Row Type) =
  ( plutipPort :: Maybe UInt
  , ogmiosPort :: Maybe UInt
  , kupoPort :: Maybe UInt
  | r
  )

type ClusterPortsOptions = Record (ClusterPortsOptions_ ())

type NoHeadless_ (r :: Row Type) = (noHeadless :: Boolean | r)

type Tests_ (r :: Row Type) = (tests :: Array E2ETest | r)

type TestTimeout_ (r :: Row Type) = (testTimeout :: Maybe Int | r)

-- | CLI options for both E2E tests and the `browser` command
type CommonOptions_ (r :: Row Type) =
  ( browser :: Maybe Browser
  , wallets :: Map WalletExt ExtensionOptions
  , chromeUserDataDir :: Maybe ChromeUserDataDir
  , tmpDir :: Maybe TmpDir
  , settingsArchive :: Maybe SettingsArchive
  , settingsArchiveUrl :: Maybe SettingsArchiveUrl
  , extraBrowserArgs :: Array BrowserArg
  )

-- | CLI options for the `browser` command.
type BrowserOptions = Record (CommonOptions_ ())

-- | Wallet extension options. Everything is wrapped in maybe, because we want
-- | to be able to override every single environment variable via the CLI.
type ExtensionOptions =
  { crxFile :: Maybe CrxFilePath
  , password :: Maybe WalletPassword
  , extensionId :: Maybe ExtensionId
  , crxUrl :: Maybe URL
  }

-- | CLI options for `pack` and `unpack` commands.
type SettingsOptions =
  { chromeUserDataDir :: Maybe FilePath
  , settingsArchive :: Maybe FilePath
  , settingsArchiveUrl :: Maybe SettingsArchiveUrl
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
  { chromeUserDataDir
  , settingsArchive
  , settingsArchiveUrl
  } <- settingsOptionsParser
  tmpDir <- option (Just <$> str) $ fold
    [ long "tmp-dir"
    , help "Temporary data directory"
    , value Nothing
    , showDefaultWith case _ of
        Nothing -> "None"
        Just a -> show a
    , metavar "DIR"
    ]
  extraBrowserArgs <- map (foldMap String.words)
    $ option (Just <$> str)
    $ fold
        [ long "extra-browser-args"
        , help
            "Extra browser CLI command arguments, combined with E2E_EXTRA_BROWSER_ARGS env variable"
        , value Nothing
        , showDefaultWith (maybe "E2E_EXTRA_BROWSER_ARGS" show)
        , metavar "CHROMIUM CLI ARGUMENT"
        ]

  nami <- parseWallet "Nami"
  eternl <- parseWallet "Eternl"
  gero <- parseWallet "Gero"
  flint <- parseWallet "Flint"
  lode <- parseWallet "Lode"

  let
    wallets = Map.fromFoldable $ catMaybes
      [ mkConfig NamiExt nami.extensionId nami.password nami.crxFile nami.crxUrl
      , mkConfig GeroExt gero.extensionId gero.password gero.crxFile gero.crxUrl
      , mkConfig FlintExt flint.extensionId flint.password flint.crxFile
          flint.crxUrl
      , mkConfig LodeExt lode.extensionId lode.password lode.crxFile lode.crxUrl
      , mkConfig EternlExt eternl.extensionId eternl.password eternl.crxFile
          eternl.crxUrl
      ]
  in
    { browser
    , wallets
    , chromeUserDataDir
    , tmpDir
    , settingsArchive
    , settingsArchiveUrl
    , extraBrowserArgs
    }
  where
  mkConfig
    :: WalletExt
    -> Maybe ExtensionId
    -> Maybe WalletPassword
    -> Maybe CrxFilePath
    -> Maybe URL
    -> Maybe (WalletExt /\ ExtensionOptions)
  mkConfig _ Nothing Nothing Nothing Nothing = Nothing
  mkConfig ext extensionId password crxFile crxUrl =
    Just $ ext /\
      { crxFile
      , password: password
      , extensionId
      , crxUrl
      }

parseWallet :: String -> Parser ExtensionOptions
parseWallet wallet = ado
  extid <- option (Just <$> extensionIdParser) $ fold
    [ long $ formattedWallet <> "-extid"
    , metavar "EXTID"
    , help $ wallet <> " extension ID"
    , value Nothing
    ]
  password <- option (Just <$> str) $ fold
    [ long $ formattedWallet <> "-password"
    , metavar "PASSWORD"
    , help $ wallet <> " wallet password"
    , value Nothing
    ]
  crx <- option (Just <$> str) $ fold
    [ long $ formattedWallet <> "-crx"
    , metavar "FILE"
    , help $ wallet <> " wallet extension (.crx) file"
    , value Nothing
    ]
  crxUrl <- option (Just <$> str) $ fold
    [ long $ formattedWallet <> "-crx-url"
    , metavar "URL"
    , help $ wallet <>
        " wallet extension (.crx) URL to download from if the file is not present"
    , value Nothing
    ]
  in { crxFile: crx, password: password, extensionId: extid, crxUrl: crxUrl }
  where
  formattedWallet = toLower wallet

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
  (clusterPorts :: ClusterPortsOptions) <- clusterPortsOptionsParser
  in
    build (merge clusterPorts <<< merge res)
      { noHeadless, tests, testTimeout }

testParser :: ReadM E2ETest
testParser = eitherReader \str ->
  note
    ( "Unable to parse test specification from: " <> str
    )
    $ mkE2ETest str

uintParser :: ReadM UInt
uintParser = eitherReader \str ->
  note "Unable to parse unsigned integer" $ UInt.fromString str

defaultPorts
  :: { kupo :: Int
     , ogmios :: Int
     , plutip :: Int
     }
defaultPorts =
  { plutip: 8087
  , ogmios: 1345
  , kupo: 1443
  }

clusterPortsOptionsParser :: Parser ClusterPortsOptions
clusterPortsOptionsParser = ado
  plutipPort <- option (Just <$> uintParser) $ fold
    [ long "plutip-port"
    , help "Plutip port for use with local cluster"
    , value Nothing
    , showDefaultWith $ const $
        showPort "PLUTIP" defaultPorts.plutip
    , metavar "PORT"
    ]
  ogmiosPort <- option (Just <$> uintParser) $ fold
    [ long "ogmios-port"
    , help "Ogmios port for use with local Plutip cluster"
    , value Nothing
    , showDefaultWith $ const $
        showPort "OGMIOS" defaultPorts.ogmios
    , metavar "PORT"
    ]
  kupoPort <- option (Just <$> uintParser) $ fold
    [ long "kupo-port"
    , help "Kupo port for use with local Plutip cluster"
    , value Nothing
    , showDefaultWith $ const $
        showPort "KUPO" defaultPorts.kupo
    , metavar "PORT"
    ]
  in
    { plutipPort
    , ogmiosPort
    , kupoPort
    }
  where
  showPort :: String -> Int -> String
  showPort service defaultPort =
    service <> "_PORT or " <> show defaultPort

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

chromeUserDataOptionParser
  :: Parser (Maybe ChromeUserDataDir)
chromeUserDataOptionParser = ado
  dataDir <- option (Just <$> str) $ fold
    [ long "chrome-user-data"
    , help "Chrome/-ium user data dir"
    , value Nothing
    , showDefaultWith $ const "E2E_CHROME_USER_DATA"
    , metavar "DIR"
    ]
  in dataDir

settingsOptionsParser :: Parser SettingsOptions
settingsOptionsParser = ado
  chromeUserDataDir <- chromeUserDataOptionParser
  settingsArchive <- option (Just <$> str) $ fold
    [ long "settings-archive"
    , help "Settings archive (.tar.gz) that will be used to store the settings"
    , value Nothing
    , showDefault
    , metavar "DIR"
    ]
  settingsArchiveUrl <- option (Just <$> str) $ fold
    [ long "settings-archive-url"
    , help
        "Settings archive (.tar.gz) URL that will be used to store the settings"
    , value Nothing
    , showDefault
    , metavar "URL"
    ]

  in
    { chromeUserDataDir
    , settingsArchive
    , settingsArchiveUrl
    }

parseOptions :: Effect TestOptions
parseOptions = execParser $ info testOptionsParser fullDesc
