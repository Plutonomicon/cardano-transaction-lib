module Ctl.Internal.Wallet.Key
  ( module KeyWallet
  ) where

import Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , getPrivatePaymentKey
  , getPrivateStakeKey
  , privateKeysToAddress
  , privateKeysToKeyWallet
  ) as KeyWallet
