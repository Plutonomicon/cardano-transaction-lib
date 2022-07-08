-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKey
  , withKeyWallet
  , module ContractAddress
  , module Wallet
  , module Serialization
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral) as ContractAddress
import Contract.Monad (Contract, ContractConfig(ContractConfig))
import Control.Monad.Reader (local)
import Data.Maybe (Maybe(Just))
import Data.Newtype (over)
import Serialization (privateKeyFromBytes) as Serialization
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami)
  , mkNamiWalletAff
  , mkGeroWalletAff
  ) as Wallet
import Wallet (mkKeyWallet, Wallet(KeyWallet))
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Wallet.Key (PrivatePaymentKey, PrivateStakeKey)

mkKeyWalletFromPrivateKey
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet

withKeyWallet
  :: forall (r :: Row Type) (a :: Type)
   . Wallet.KeyWallet
  -> Contract r a
  -> Contract r a
withKeyWallet wallet action = do
  let
    setUpdatedWallet =
      over ContractConfig _ { wallet = Just $ KeyWallet wallet }
  local setUpdatedWallet action
