module Ctl.Internal.Wallet.Bip32
  ( Cip1852Account
  , bip32ToPrivateKey
  , bip32PrivateKeyFromMnemonic
  , cip1852AccountFromBip32PrivateKey
  , cip1852AccountFromMnemonic
  , derivePaymentKey
  , deriveChangeKey
  , deriveStakeKey
  ) where

import Contract.Prelude

import Ctl.Internal.Serialization.Types (Bip32PrivateKey, PrivateKey)
import Data.UInt (UInt)
import Data.UInt as UInt

newtype Cip1852Account = Cip1852Account Bip32PrivateKey

foreign import _bip32PrivateKeyFromMnemonic
  :: (String -> Either String Bip32PrivateKey)
  -> (Bip32PrivateKey -> Either String Bip32PrivateKey)
  -> String
  -> Either String Bip32PrivateKey

foreign import derivePrivateKey
  :: UInt
  -- ^ path index
  -> Boolean
  -- ^ hardened
  -> Bip32PrivateKey
  -> Bip32PrivateKey

-- | Convert a BIP32 private key to a raw private key
foreign import bip32ToPrivateKey :: Bip32PrivateKey -> PrivateKey

-- | Derive a BIP32 private key given a mnemonic phrase
bip32PrivateKeyFromMnemonic :: String -> Either String Bip32PrivateKey
bip32PrivateKeyFromMnemonic = _bip32PrivateKeyFromMnemonic Left Right

-- | Derive a CIP1852 account from a BIP32 private key given an account index
cip1852AccountFromBip32PrivateKey :: UInt -> Bip32PrivateKey -> Cip1852Account
cip1852AccountFromBip32PrivateKey account key =
  Cip1852Account
    $ key
    # derivePrivateKey (UInt.fromInt 1852) true
    # derivePrivateKey (UInt.fromInt 1815) true
    # derivePrivateKey account true

-- | Derive a CIP1852 account given a mnemonic phrase and account index
cip1852AccountFromMnemonic :: String -> UInt -> Either String Cip1852Account
cip1852AccountFromMnemonic phrase account =
  cip1852AccountFromBip32PrivateKey account
    <$> bip32PrivateKeyFromMnemonic phrase

-- | Derive a payment key for the given account
derivePaymentKey :: Cip1852Account -> UInt -> Bip32PrivateKey
derivePaymentKey (Cip1852Account key) index =
  key
    # derivePrivateKey zero false
    # derivePrivateKey index false

-- | Derive a change key for the given account
deriveChangeKey :: Cip1852Account -> UInt -> Bip32PrivateKey
deriveChangeKey (Cip1852Account key) index =
  key
    # derivePrivateKey one false
    # derivePrivateKey index false

-- | Derive the stake key for the given account
deriveStakeKey :: Cip1852Account -> Bip32PrivateKey
deriveStakeKey (Cip1852Account key) =
  key
    # derivePrivateKey (UInt.fromInt 2) false
    # derivePrivateKey (UInt.fromInt 0) false
