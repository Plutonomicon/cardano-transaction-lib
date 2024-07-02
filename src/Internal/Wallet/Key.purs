module Ctl.Internal.Wallet.Key
  ( module KeyWallet
  ) where

import Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , privateKeysToAddress
  , privateKeysToKeyWallet
  ) as KeyWallet
