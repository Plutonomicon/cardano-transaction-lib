-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKey
  , module ContractAddress
  , module Wallet
  ) where

import Contract.Address (getWalletAddress, getWalletCollateral) as ContractAddress
import Serialization.Types (PrivateKey)
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami)
  , mkNamiWalletAff
  , mkGeroWalletAff
  ) as Wallet
import Wallet (mkKeyWallet)
import Wallet.Key (KeyWallet, privateKeyFromNormalBytes, privateKeyToKeyWallet) as Wallet

mkKeyWalletFromPrivateKey :: PrivateKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet
