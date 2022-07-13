-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( module Contract.Address
  , module Serialization
  , module Wallet.Spec
  , module Wallet.Key
  ) where

import Contract.Address (getWalletAddress, getWalletCollateral)
import Serialization (privateKeyFromBytes)
import Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Wallet.Spec
  ( WalletSpec(UseKeys, ConnectToNami, ConnectToGero)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )
