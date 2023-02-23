-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , module Contract.Address
  , module Contract.Utxos
  , module X
  , module Deserialization.Keys
  , module Wallet
  , module Ctl.Internal.Wallet.Key
  , module Ctl.Internal.Wallet.KeyFile
  , module Ctl.Internal.Wallet.Spec
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (Contract)
import Contract.Utxos (getWalletUtxos) as Contract.Utxos
import Control.Monad.Reader (local)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , signData
  ) as X
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes) as Deserialization.Keys
import Ctl.Internal.Wallet
  ( Wallet(Gero, Nami, Flint, Lode, Eternl, KeyWallet)
  , WalletExtension
  , apiVersion
  , icon
  , isEnabled
  , isEternlAvailable
  , isFlintAvailable
  , isGeroAvailable
  , isLodeAvailable
  , isNamiAvailable
  , isWalletAvailable
  , name
  , walletToWalletExtension
  ) as Wallet
import Ctl.Internal.Wallet (Wallet(KeyWallet))
import Ctl.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  )
import Ctl.Internal.Wallet.KeyFile (formatPaymentKey, formatStakeKey)
import Ctl.Internal.Wallet.Spec
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToLode
      , ConnectToEternl
      , ConnectToNuFi
      )
  )
import Data.Maybe (Maybe(Just))

withKeyWallet
  :: forall (a :: Type)
   . Wallet.KeyWallet
  -> Contract a
  -> Contract a
withKeyWallet wallet =
  local _ { wallet = Just $ KeyWallet wallet }

mkKeyWalletFromPrivateKeys
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet.KeyWallet
mkKeyWalletFromPrivateKeys payment mbStake = privateKeysToKeyWallet payment
  mbStake
