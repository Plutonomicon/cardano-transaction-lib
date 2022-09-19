module Contract.Test.E2E.Types where

import Data.Maybe
import Prelude

import Data.Map (Map)

type BrowserPath = String

type TempDir = String

type SettingsArchive = String

type ChromeUserDataDir = String

type ExtensionParams =
  { crx :: String
  , password :: String
  , extensionId :: String
  }

data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt | EternlExt

derive instance Eq WalletExt
derive instance Ord WalletExt

type Extensions = Map WalletExt ExtensionParams
