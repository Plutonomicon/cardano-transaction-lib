module Contract.Test.E2E.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Node.Path (FilePath)
import Toppokki as Toppokki

type BrowserPath = String

type TempDir = String

type SettingsArchive = String

type ChromeUserDataDir = String

type CrxFilePath = FilePath

type ExtensionId = String

type WalletPassword = String

type ExtensionParams =
  { crx :: CrxFilePath
  , password :: WalletPassword
  , extensionId :: ExtensionId
  }

data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt | EternlExt

derive instance Eq WalletExt
derive instance Ord WalletExt
derive instance Generic WalletExt _

instance Show WalletExt where
  show = genericShow

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
