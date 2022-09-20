module Contract.Test.E2E.Options where

import Prelude

import Contract.Test.E2E.Helpers (WalletPassword(WalletPassword))
import Contract.Test.E2E.Types (ExtensionParams, CrxFilePath)
import Contract.Test.E2E.WalletExt
  ( ExtensionId(ExtensionId)
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  )
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
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
import Options.Applicative.Builder (info)
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
  eternlExtId <- option (Just <<< ExtensionId <$> str) $ fold
    [ long "eternl-extid"
    , metavar "EXTID"
    , help "Eternl extension ID"
    , value Nothing
    ]
  eternlPassword <- option (Just <<< WalletPassword <$> str) $ fold
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
  namiExtId <- option (Just <<< ExtensionId <$> str) $ fold
    [ long "nami-extid"
    , metavar "EXTID"
    , help "Nami extension ID"
    , value Nothing
    ]
  namiPassword <- option (Just <<< WalletPassword <$> str) $ fold
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
  geroExtId <- option (Just <<< ExtensionId <$> str) $ fold
    [ long "gero-extid"
    , metavar "EXTID"
    , help "Gero extension ID"
    , value Nothing
    ]
  geroPassword <- option (Just <<< WalletPassword <$> str) $ fold
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
  flintExtId <- option (Just <<< ExtensionId <$> str) $ fold
    [ long "flint-extid"
    , metavar "EXTID"
    , help "Flint extension ID"
    , value Nothing
    ]
  flintPassword <- option (Just <<< WalletPassword <$> str) $ fold
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
  lodeExtId <- option (Just <<< ExtensionId <$> str) $ fold
    [ long "lode-extid"
    , metavar "EXTID"
    , help "Lode extension ID"
    , value Nothing
    ]
  lodePassword <- option (Just <<< WalletPassword <$> str) $ fold
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
