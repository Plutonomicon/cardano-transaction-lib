-- | Common types for E2E tests.
module Ctl.Internal.Test.E2E.Types
  ( Browser
  , TmpDir
  , SettingsArchive
  , ChromeUserDataDir
  , CrxFilePath
  , ExtensionId
  , mkExtensionId
  , unExtensionId
  , WalletPassword
  , ExtensionParams
  , WalletExt(FlintExt, NamiExt, GeroExt, LodeExt, EternlExt)
  , Extensions
  , E2ETestRuntime
  , SettingsRuntime
  , E2ETest
  , mkE2ETest
  , RunningE2ETest
  , SomeWallet
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.Regex (regex, test) as Regex
import Data.String.Regex.Flags (global) as Regex
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Toppokki as Toppokki

-- | Browser binary path or short name
type Browser = FilePath

-- | Temporary directory prefix
type TmpDir = FilePath

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
  , chromeUserDataDir :: ChromeUserDataDir
  , tmpDir :: TmpDir
  , browser :: Browser
  , settingsArchive :: SettingsArchive
  }

-- | Contains everything needed to pack and unpack settings.
type SettingsRuntime =
  { chromeUserDataDir :: ChromeUserDataDir
  , settingsArchive :: SettingsArchive
  }

-- | A particular test instance.
type E2ETest =
  { url :: String
  , wallet :: WalletExt
  }

-- | Construct an `E2ETest` from a spec (`wallet:url`)
mkE2ETest :: String -> Maybe E2ETest
mkE2ETest str =
  (stripPrefix (Pattern "eternl:") str <#> mkTestEntry EternlExt)
    <|> (stripPrefix (Pattern "flint:") str <#> mkTestEntry FlintExt)
    <|> (stripPrefix (Pattern "gero:") str <#> mkTestEntry GeroExt)
    <|> (stripPrefix (Pattern "lode:") str <#> mkTestEntry LodeExt)
    <|> (stripPrefix (Pattern "nami:") str <#> mkTestEntry NamiExt)
  where
  mkTestEntry wallet url = { wallet, url }

-- | Represents a connection to a running E2E test.
type RunningE2ETest =
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
  , confirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
  , sign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
  }
