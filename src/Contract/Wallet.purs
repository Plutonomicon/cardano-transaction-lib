-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , getNetworkId
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , module Contract.Address
  , module Contract.Utxos
  , module Serialization
  , module Wallet
  , module Wallet.Key
  , module Wallet.KeyFile
  , module Wallet.Spec
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (Contract, ContractEnv, wrapContract)
import Contract.Utxos (getWalletUtxos) as Contract.Utxos
import Control.Monad.Reader (local)
import Data.Lens (Lens, (.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just))
import QueryM (getChangeAddress, getNetworkId, getRewardAddresses, getUnusedAddresses) as QueryM
import Serialization (privateKeyFromBytes) as Serialization
import Type.Proxy (Proxy(Proxy))
import Wallet (isGeroAvailable, isNamiAvailable, isFlintAvailable, isEternlAvailable, isLodeAvailable) as Wallet
import Wallet (mkKeyWallet, Wallet(KeyWallet))
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Wallet.Key (PrivatePaymentKey(PrivatePaymentKey), PrivateStakeKey(PrivateStakeKey))
import Wallet.KeyFile (formatPaymentKey, formatStakeKey)
import Wallet.Spec (WalletSpec(UseKeys, ConnectToNami, ConnectToGero, ConnectToFlint, ConnectToLode, ConnectToEternl), PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue), PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue))
import Serialization.Address (Address)

getNetworkId :: forall (r::Row Type) . Contract r Int 
getNetworkId = wrapContract  QueryM.getNetworkId

getUnusedAddresses :: forall (r::Row Type) . Contract r (Maybe (Array Address)) 
getUnusedAddresses = wrapContract QueryM.getUnusedAddresses

getChangeAddress :: forall (r::Row Type) . Contract r (Maybe Address) 
getChangeAddress = wrapContract QueryM.getChangeAddress

getRewardAddresses :: forall (r::Row Type) . Contract r (Maybe (Array Address)) 
getRewardAddresses = wrapContract QueryM.getRewardAddresses




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
