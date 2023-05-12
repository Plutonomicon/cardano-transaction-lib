module Ctl.Internal.Wallet.Bip32
  ( Bip32Account
  , bip32AccountFromMnemonic
  , derivePaymentKey
  , deriveChangeKey
  , deriveStakeKey
  , mkKeyWalletSpecFromMnemonic
  , mkWalletFromMnemonic
  ) where

import Contract.Prelude

import Contract.Config
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , PrivateStakeKey(PrivateStakeKey)
  , PrivateStakeKeySource(PrivateStakeKeyValue)
  , WalletSpec(UseKeys)
  )
import Contract.Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Ctl.Internal.Serialization.Types (Bip32PrivateKey, PrivateKey)
import Data.UInt (UInt)
import Data.UInt as UInt

newtype Bip32Account = Bip32Account Bip32PrivateKey

foreign import bip32PrivateKeyFromMnemonic
  :: (String -> Either String Bip32PrivateKey)
  -> (Bip32PrivateKey -> Either String Bip32PrivateKey)
  -> String
  -> Either String Bip32PrivateKey

foreign import bip32ToPrivateKey :: Bip32PrivateKey -> PrivateKey

foreign import derivePrivateKey
  :: UInt
  -- ^ path index
  -> Boolean
  -- ^ hardened
  -> Bip32PrivateKey
  -> Bip32PrivateKey

-- | Derive a BIP32 account given a mnemonic phrase and account index
bip32AccountFromMnemonic :: String -> UInt -> Either String Bip32Account
bip32AccountFromMnemonic phrase account =
  bip32PrivateKeyFromMnemonic Left Right phrase
    <#> derivePrivateKey (UInt.fromInt 1852) true
    <#> derivePrivateKey (UInt.fromInt 1815) true
    <#> derivePrivateKey account true
    <#> Bip32Account

-- | Derive a payment key for the given account
derivePaymentKey :: Bip32Account -> UInt -> Bip32PrivateKey
derivePaymentKey (Bip32Account key) index =
  key
    # derivePrivateKey zero false
    # derivePrivateKey index false

-- | Derive a change key for the given account
deriveChangeKey :: Bip32Account -> UInt -> Bip32PrivateKey
deriveChangeKey (Bip32Account key) index =
  key
    # derivePrivateKey one false
    # derivePrivateKey index false

-- | Derive the stake key for the given account
deriveStakeKey :: Bip32Account -> Bip32PrivateKey
deriveStakeKey (Bip32Account key) =
  key
    # derivePrivateKey (UInt.fromInt 2) false
    # derivePrivateKey (UInt.fromInt 0) false

-- | Create a key wallet spec given a mnemonic phrase and account index
mkKeyWalletSpecFromMnemonic :: String -> UInt -> Either String WalletSpec
mkKeyWalletSpecFromMnemonic phrase accountIndex = do
  account <- bip32AccountFromMnemonic phrase accountIndex
  let
    paymentKey = derivePaymentKey account zero # bip32ToPrivateKey
    stakeKey = deriveStakeKey account # bip32ToPrivateKey
  pure $
    UseKeys
      (PrivatePaymentKeyValue $ PrivatePaymentKey paymentKey)
      (Just $ PrivateStakeKeyValue $ PrivateStakeKey stakeKey)

-- | Create a wallet given a mnemonic phrase and account index
mkWalletFromMnemonic :: String -> UInt -> Either String KeyWallet
mkWalletFromMnemonic phrase accountIndex = do
  account <- bip32AccountFromMnemonic phrase accountIndex
  let
    paymentKey = derivePaymentKey account zero # bip32ToPrivateKey
    stakeKey = deriveStakeKey account # bip32ToPrivateKey
  pure $
    privateKeysToKeyWallet
      (PrivatePaymentKey paymentKey)
      (Just $ PrivateStakeKey stakeKey)
