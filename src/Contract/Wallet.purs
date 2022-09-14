-- | A module with Wallet-related functionality.
module CTL.Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , module CTL.Contract.Address
  , module Contract.Utxos
  , module Serialization
  , module Wallet
  , module CTL.Internal.Wallet.Key
  , module CTL.Internal.Wallet.KeyFile
  , module CTL.Internal.Wallet.Spec
  ) where

import Prelude

import CTL.Contract.Address (getWalletAddress, getWalletCollateral)
import CTL.Contract.Monad (Contract, ContractEnv)
import CTL.Contract.Utxos (getWalletUtxos) as Contract.Utxos
import CTL.Internal.Serialization (privateKeyFromBytes) as Serialization
import CTL.Internal.Wallet (Wallet(KeyWallet), mkKeyWallet)
import CTL.Internal.Wallet
  ( isFlintAvailable
  , isGeroAvailable
  , isLodeAvailable
  , isNamiAvailable
  ) as Wallet
import CTL.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import CTL.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import CTL.Internal.Wallet.KeyFile (formatPaymentKey, formatStakeKey)
import CTL.Internal.Wallet.Spec
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec
      ( UseKeys
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToLode
      )
  )
import Control.Monad.Reader (local)
import Data.Lens (Lens, (.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just))
import Type.Proxy (Proxy(Proxy))

withKeyWallet
  :: forall (r :: Row Type) (a :: Type)
   . Wallet.KeyWallet
  -> Contract r a
  -> Contract r a
withKeyWallet wallet action = do
  let
    setUpdatedWallet :: ContractEnv r -> ContractEnv r
    setUpdatedWallet =
      simple _Newtype <<< _runtime <<< _wallet .~
        (Just (KeyWallet wallet))
  local setUpdatedWallet action
  where
  _wallet
    :: forall x rest. Lens { wallet :: x | rest } { wallet :: x | rest } x x
  _wallet = prop (Proxy :: Proxy "wallet")

  _runtime
    :: forall x rest. Lens { runtime :: x | rest } { runtime :: x | rest } x x
  _runtime = prop (Proxy :: Proxy "runtime")

mkKeyWalletFromPrivateKeys
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWalletFromPrivateKeys = mkKeyWallet
