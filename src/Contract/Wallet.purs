-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKey
  , module ContractAddress
  , module Wallet
  , module Serialization
  ) where

import Contract.Address (getWalletAddress, getWalletCollateral) as ContractAddress
import Data.Maybe (Maybe)
import Serialization (privateKeyFromBytes) as Serialization
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami)
  , isGeroAvailable
  , isNamiAvailable
  , mkGeroWalletAff
  , mkNamiWalletAff
  ) as Wallet
import Wallet (mkKeyWallet)
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Wallet.Key (PrivatePaymentKey, PrivateStakeKey)

mkKeyWalletFromPrivateKey
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet
