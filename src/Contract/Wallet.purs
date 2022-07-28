-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( module Contract.Address
  , module Serialization
  , module Wallet.Spec
  , module Wallet.Key
  , module Wallet
  , mkKeyWalletFromPrivateKeys
  ) where

import Contract.Address (getWalletAddress, getWalletCollateral)
import Wallet.Spec
  ( WalletSpec(UseKeys, ConnectToNami, ConnectToGero)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )
import Data.Maybe (Maybe)
import Serialization (privateKeyFromBytes) as Serialization
import Wallet
  ( isGeroAvailable
  , isNamiAvailable
  ) as Wallet
import Wallet (Wallet, mkKeyWallet)
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Wallet.Key (PrivatePaymentKey, PrivateStakeKey)

mkKeyWalletFromPrivateKeys
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWalletFromPrivateKeys = mkKeyWallet
