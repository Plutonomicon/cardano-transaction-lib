module Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , KnownWallet(Nami, Gero, Flint, Eternl, Lode, Lace, NuFi)
  , MnemonicSource(MnemonicString, MnemonicFile)
  , PrivateDrepKeySource(PrivateDrepKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , WalletSpec(UseKeys, UseMnemonic, ConnectToGenericCip30)
  , mkWalletBySpec
  , mkKeyWalletFromMnemonic
  , walletName
  ) where

import Prelude

import Cardano.Wallet.HD
  ( bip32ToPrivateKey
  , cip1852AccountFromMnemonic
  , deriveDrepKey
  , derivePaymentKey
  , deriveStakeKey
  )
import Cardano.Wallet.Key
  ( KeyWallet
  , PrivateDrepKey(PrivateDrepKey)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  )
import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Wallet
  ( Cip30Extensions
  , Wallet(KeyWallet)
  , mkKeyWallet
  , mkWalletAff
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

data PrivateDrepKeySource = PrivateDrepKeyValue PrivateDrepKey

derive instance Generic PrivateDrepKeySource _

instance Show PrivateDrepKeySource where
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
      (Maybe PrivateDrepKeySource)
  | UseMnemonic MnemonicSource Cip1852DerivationPath StakeKeyPresence
  | ConnectToGenericCip30 String Cip30Extensions

derive instance Generic WalletSpec _

instance Show WalletSpec where
  show = genericShow

data KnownWallet = Nami | Gero | Flint | Eternl | Lode | Lace | NuFi

walletName :: KnownWallet -> String
walletName = case _ of
  Nami -> "nami"
  Gero -> "gerowallet"
  Flint -> "flint"
  Eternl -> "eternl"
  Lode -> "LodeWallet"
  Lace -> "lace"
  NuFi -> "nufi"

-- | Contains non-constant parameters for a CIP-1852 derivation path.
-- | See https://cips.cardano.org/cips/cip1852/ and `doc/key-management.md`.
type Cip1852DerivationPath =
  { accountIndex :: UInt
  , addressIndex :: UInt
  }

mkWalletBySpec :: WalletSpec -> Aff Wallet
mkWalletBySpec = case _ of
  UseKeys paymentKeySpec mbStakeKeySpec mbDrepKeySpec -> do
    privatePaymentKey <- case paymentKeySpec of
      PrivatePaymentKeyFile filePath ->
        privatePaymentKeyFromFile filePath
      PrivatePaymentKeyValue key -> pure key
    mbPrivateStakeKey <- for mbStakeKeySpec case _ of
      PrivateStakeKeyFile filePath -> privateStakeKeyFromFile filePath
      PrivateStakeKeyValue key -> pure key
    mbDrepKey <- for mbDrepKeySpec case _ of
      PrivateDrepKeyValue key -> pure key
    pure $ mkKeyWallet privatePaymentKey mbPrivateStakeKey mbDrepKey
  UseMnemonic (MnemonicString mnemonic) derivationPath stakeKeyPresence -> do
    map KeyWallet $ liftEither $ lmap error $
      mkKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence
  UseMnemonic (MnemonicFile path) derivationPath stakeKeyPresence -> do
    mnemonic <- readTextFile Encoding.UTF8 path
    map KeyWallet $ liftEither $ lmap error $
      mkKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence
  ConnectToGenericCip30 name exts -> mkWalletAff { name, exts }

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
      paymentKey =
        PrivatePaymentKey $ bip32ToPrivateKey $ derivePaymentKey account
          addressIndex
      drepKey = Just $ PrivateDrepKey $ bip32ToPrivateKey $ deriveDrepKey
        account
      mbStakeKey = case stakeKeyPresence of
        WithStakeKey -> Just $ PrivateStakeKey $
          deriveStakeKey account #
            bip32ToPrivateKey
        WithoutStakeKey -> Nothing
    pure $ privateKeysToKeyWallet paymentKey mbStakeKey drepKey
