module Contract.Test.E2E.Types where

import Data.Maybe

type BrowserPath = String

type TempDir = String

type SettingsArchive = String

type ChromeUserDataDir = String

type ExtensionParams =
  { crx :: String
  , password :: String
  , extensionId :: String
  }

type Extensions =
  { nami :: Maybe ExtensionParams
  , flint :: Maybe ExtensionParams
  , gero :: Maybe ExtensionParams
  , lode :: Maybe ExtensionParams
  , eternl :: Maybe ExtensionParams
  }
