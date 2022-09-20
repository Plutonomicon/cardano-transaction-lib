module Contract.Test.E2E.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Node.Path (FilePath)

type BrowserPath = String

type TempDir = String

type SettingsArchive = String

type ChromeUserDataDir = String

type CrxFilePath = FilePath

type ExtensionParams =
  { crx :: String -- TODO: crxFile
  , password :: WalletPassword
  , extensionId :: ExtensionId
  }

newtype WalletPassword = WalletPassword String

derive instance Newtype WalletPassword _

derive instance Generic WalletPassword _

instance Show WalletPassword where
  show = genericShow

data WalletExt = FlintExt | NamiExt | GeroExt | LodeExt | EternlExt

derive instance Eq WalletExt
derive instance Ord WalletExt
derive instance Generic WalletExt _

instance Show WalletExt where
  show = genericShow

type Extensions = Map WalletExt ExtensionParams

newtype ExtensionId = ExtensionId String

derive instance Newtype ExtensionId _

derive instance Generic ExtensionId _

instance Show ExtensionId where
  show = genericShow
