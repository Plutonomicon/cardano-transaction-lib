module Ctl.Internal.Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      )
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  ) where

import Ctl.Internal.Wallet.Key (PrivatePaymentKey, PrivateStakeKey)
import Data.Maybe (Maybe)
import Node.Path (FilePath)

data PrivatePaymentKeySource
  = PrivatePaymentKeyFile FilePath
  | PrivatePaymentKeyValue PrivatePaymentKey

data PrivateStakeKeySource
  = PrivateStakeKeyFile FilePath
  | PrivateStakeKeyValue PrivateStakeKey

-- | A data type to describe instructions on how to initialize a wallet.
data WalletSpec
  = UseKeys PrivatePaymentKeySource (Maybe PrivateStakeKeySource)
  | ConnectToNami
  | ConnectToGero
  | ConnectToFlint
  | ConnectToEternl
  | ConnectToLode
