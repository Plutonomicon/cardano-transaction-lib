module Ctl.Internal.Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , UseMnemonic
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToNuFi
      , ConnectToLace
      , ConnectToGenericCip30
      )
  , Cip1852DerivationPath
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , MnemonicSource(MnemonicString, MnemonicFile)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , mkWalletBySpec
  , mkKeyWalletFromMnemonic
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet)
  , WalletExtension
      ( NamiWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
      , LodeWallet
      , NuFiWallet
      , LaceWallet
      , GenericCip30Wallet
      )
  , mkKeyWallet
  , mkWalletAff
  )
import Ctl.Internal.Wallet.Bip32
  ( bip32ToPrivateKey
  , cip1852AccountFromMnemonic
  , derivePaymentKey
  , deriveStakeKey
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  )
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Encoding as Encoding
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

data PrivatePaymentKeySource
  = PrivatePaymentKeyFile FilePath
  | PrivatePaymentKeyValue PrivatePaymentKey

derive instance Generic PrivatePaymentKeySource _

instance Show PrivatePaymentKeySource where
  show = genericShow

data PrivateStakeKeySource
  = PrivateStakeKeyFile FilePath
  | PrivateStakeKeyValue PrivateStakeKey

derive instance Generic PrivateStakeKeySource _

instance Show PrivateStakeKeySource where
  show = genericShow

data MnemonicSource
  = MnemonicString String
  | MnemonicFile FilePath

derive instance Generic MnemonicSource _

instance Show MnemonicSource where
  show = genericShow

data StakeKeyPresence = WithStakeKey | WithoutStakeKey

derive instance Generic StakeKeyPresence _

instance Show StakeKeyPresence where
  show = genericShow

-- | A data type to describe instructions on how to initialize a wallet.
data WalletSpec
  = UseKeys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)
  | UseMnemonic MnemonicSource Cip1852DerivationPath StakeKeyPresence
  | ConnectToNami
  | ConnectToGero
  | ConnectToFlint
  | ConnectToEternl
  | ConnectToLode
  | ConnectToNuFi
  | ConnectToLace
  | ConnectToGenericCip30 String

derive instance Generic WalletSpec _

instance Show WalletSpec where
  show = genericShow

-- | Contains non-constant parameters for a CIP-1852 derivation path.
-- | See https://cips.cardano.org/cips/cip1852/ and `doc/key-management.md`.
type Cip1852DerivationPath =
  { accountIndex :: UInt
  , addressIndex :: UInt
  }

mkWalletBySpec :: WalletSpec -> Aff Wallet
mkWalletBySpec = case _ of
  UseKeys paymentKeySpec mbStakeKeySpec -> do
    privatePaymentKey <- case paymentKeySpec of
      PrivatePaymentKeyFile filePath ->
        privatePaymentKeyFromFile filePath
      PrivatePaymentKeyValue key -> pure key
    mbPrivateStakeKey <- for mbStakeKeySpec case _ of
      PrivateStakeKeyFile filePath -> privateStakeKeyFromFile filePath
      PrivateStakeKeyValue key -> pure key
    pure $ mkKeyWallet privatePaymentKey mbPrivateStakeKey
  UseMnemonic (MnemonicString mnemonic) derivationPath stakeKeyPresence -> do
    map KeyWallet $ liftEither $ lmap error $
      mkKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence
  UseMnemonic (MnemonicFile path) derivationPath stakeKeyPresence -> do
    mnemonic <- readTextFile Encoding.UTF8 path
    map KeyWallet $ liftEither $ lmap error $
      mkKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence
  ConnectToNami -> mkWalletAff NamiWallet
  ConnectToGero -> mkWalletAff GeroWallet
  ConnectToFlint -> mkWalletAff FlintWallet
  ConnectToEternl -> mkWalletAff EternlWallet
  ConnectToLode -> mkWalletAff LodeWallet
  ConnectToNuFi -> mkWalletAff NuFiWallet
  ConnectToLace -> mkWalletAff LaceWallet
  ConnectToGenericCip30 name -> mkWalletAff (GenericCip30Wallet name)

-- | Create a wallet given a mnemonic phrase, account index, address index and
-- | stake key presence flag.
-- | See `doc/key-management.md` for more info.
mkKeyWalletFromMnemonic
  :: String
  -> Cip1852DerivationPath
  -> StakeKeyPresence
  -> Either String KeyWallet
mkKeyWalletFromMnemonic phrase { accountIndex, addressIndex } stakeKeyPresence =
  do
    account <- cip1852AccountFromMnemonic phrase accountIndex
    let
      paymentKey = derivePaymentKey account addressIndex # bip32ToPrivateKey
      mbStakeKeySpec = case stakeKeyPresence of
        WithStakeKey -> Just $ PrivateStakeKey $ deriveStakeKey account #
          bip32ToPrivateKey
        WithoutStakeKey -> Nothing
    pure $ privateKeysToKeyWallet (PrivatePaymentKey paymentKey) mbStakeKeySpec
