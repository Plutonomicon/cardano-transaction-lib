-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKey
  , module ContractAddress
  , module Wallet
  , module Serialization
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
import Wallet.Key (KeyWallet, privateKeyToKeyWallet) as Wallet
import Serialization (privateKeyFromBytes) as Serialization

mkKeyWalletFromPrivateKey :: PrivateKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet
