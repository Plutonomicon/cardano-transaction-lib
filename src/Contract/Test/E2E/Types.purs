module Contract.Test.E2E.Types where

import Prelude

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.String.Regex (regex, test) as Regex
import Data.String.Regex.Flags (global) as Regex
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Toppokki as Toppokki

-- | Browser binary path
type BrowserPath = FilePath

-- | Temporary directory prefix
type TempDir = FilePath

-- | `.tar.gz` archive containing the runtime settings for all the extensions
-- | used in E2E.
type SettingsArchive = FilePath

-- | Chrome user data directory
type ChromeUserDataDir = FilePath

-- | Path to a `.crx` chrome extension file.
type CrxFilePath = FilePath

-- | Chrome Extension ID. Use `mkExtensionId` to construct.
newtype ExtensionId = ExtensionId String

-- | A smart constructor for `ExtensionId`
mkExtensionId :: String -> Maybe ExtensionId
mkExtensionId str
  | Just regex <- hush $ Regex.regex "^[a-z]{32}$" Regex.global
  , Regex.test regex str = pure $ ExtensionId str
  | otherwise = Nothing

unExtensionId :: ExtensionId -> String
unExtensionId (ExtensionId str) = str

instance Show ExtensionId where
  show (ExtensionId str) = "(unsafePartial $ fromJust $ mkExtensionId "
    <> show str
    <> ")"

-- | Password for some wallet extension.
type WalletPassword = String

-- | A record containing everything needed to use a Chrome extension in the E2E
-- | test suite.
type ExtensionParams =
  { crx :: CrxFilePath
  , password :: WalletPassword
  , extensionId :: ExtensionId
  }

-- | Enumeration of all known extensions.
data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt | EternlExt

derive instance Eq WalletExt
derive instance Ord WalletExt
derive instance Generic WalletExt _

instance Show WalletExt where
  show = genericShow

-- | Extension parameters for some set of known extensions.
type Extensions = Map WalletExt ExtensionParams

-- | Contains everything needed to run E2E tests.
type E2ETestRuntime =
  { wallets :: Map WalletExt ExtensionParams
  , chromeUserDataDir :: FilePath
  , tempDir :: FilePath
  , browserPath :: String
  , settingsArchive :: FilePath
  }

-- | Contains everything needed to pack and unpack settings.
type SettingsRuntime =
  { chromeUserDataDir :: FilePath
  , settingsArchive :: FilePath
  }

-- | A particular test instance.
type E2ETest =
  { url :: String
  , wallet :: WalletExt
  }

-- TODO: rename to runningTest
type RunningExample =
  { browser :: Toppokki.Browser
  , jQuery :: String
  , page :: Toppokki.Page
  }

-- | Provides an interface to interact with a wallet running in a headless
-- | browser.
type SomeWallet =
  { wallet :: WalletExt
  , name :: String
  , extensionId :: ExtensionId
  , confirmAccess :: ExtensionId -> RunningExample -> Aff Unit
  , sign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
  }
