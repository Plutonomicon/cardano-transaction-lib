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
      )
  , MnemonicSource(MnemonicString, MnemonicFile)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , mkWalletBySpec
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Ctl.Internal.Wallet
  ( Wallet
  , WalletExtension
      ( NamiWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
      , LodeWallet
      , NuFiWallet
      , LaceWallet
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
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(Just))
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

data PrivateStakeKeySource
  = PrivateStakeKeyFile FilePath
  | PrivateStakeKeyValue PrivateStakeKey

data MnemonicSource
  = MnemonicString String
  | MnemonicFile FilePath

-- | A data type to describe instructions on how to initialize a wallet.
data WalletSpec
  = UseKeys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)
  | UseMnemonic MnemonicSource UInt
  | ConnectToNami
  | ConnectToGero
  | ConnectToFlint
  | ConnectToEternl
  | ConnectToLode
  | ConnectToNuFi
  | ConnectToLace

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
  UseMnemonic (MnemonicString mnemonic) accountIndex -> do
    keyFromMnemonicHelper mnemonic accountIndex
  UseMnemonic (MnemonicFile path) accountIndex -> do
    mnemonic <- readTextFile Encoding.UTF8 path
    keyFromMnemonicHelper mnemonic accountIndex
  ConnectToNami -> mkWalletAff NamiWallet
  ConnectToGero -> mkWalletAff GeroWallet
  ConnectToFlint -> mkWalletAff FlintWallet
  ConnectToEternl -> mkWalletAff EternlWallet
  ConnectToLode -> mkWalletAff LodeWallet
  ConnectToNuFi -> mkWalletAff NuFiWallet
  ConnectToLace -> mkWalletAff LaceWallet

keyFromMnemonicHelper :: String -> UInt -> Aff Wallet
keyFromMnemonicHelper mnemonic accountIndex = do
  account <- liftEither $ lmap error
    $ cip1852AccountFromMnemonic mnemonic accountIndex
  let
    paymentKey = derivePaymentKey account zero # bip32ToPrivateKey
    stakeKey = deriveStakeKey account # bip32ToPrivateKey
  pure $ mkKeyWallet (PrivatePaymentKey paymentKey)
    (Just $ PrivateStakeKey stakeKey)
