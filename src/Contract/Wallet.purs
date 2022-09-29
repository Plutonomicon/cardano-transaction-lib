-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , getNetworkId
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , signData
  , isEnabled
  , module Contract.Address
  , module Contract.Utxos
  , module Serialization
  , module Wallet
  , module Ctl.Internal.Wallet.Key
  , module Ctl.Internal.Wallet.KeyFile
  , module Ctl.Internal.Wallet.Spec
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (Contract, ContractEnv, wrapContract)
import Contract.Utxos (getWalletUtxos) as Contract.Utxos
import Control.Monad.Reader (local)
import Ctl.Internal.QueryM (getChangeAddress, getNetworkId, getRewardAddresses, getUnusedAddresses, signData, isEnabled) as QueryM
import Ctl.Internal.Serialization (privateKeyFromBytes) as Serialization
import Ctl.Internal.Serialization.Address (Address)
import Ctl.Internal.Types.RawBytes (RawBytes)
import Ctl.Internal.Wallet (Wallet(KeyWallet), mkKeyWallet)
import Ctl.Internal.Wallet (isEternlAvailable, isFlintAvailable, isGeroAvailable, isLodeAvailable, isNamiAvailable, Wallet(Gero, Nami, Flint, Lode, Eternl, KeyWallet)) as Wallet
import Ctl.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Ctl.Internal.Wallet.Key (PrivatePaymentKey(PrivatePaymentKey), PrivateStakeKey(PrivateStakeKey))
import Ctl.Internal.Wallet.KeyFile (formatPaymentKey, formatStakeKey)
import Ctl.Internal.Wallet.Spec (PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue), PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue), WalletSpec(UseKeys, ConnectToNami, ConnectToGero, ConnectToFlint, ConnectToLode, ConnectToEternl))
import Data.Lens (Lens, (.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Type.Proxy (Proxy(Proxy))

getNetworkId :: forall (r :: Row Type). Contract r Int
getNetworkId = wrapContract QueryM.getNetworkId

getUnusedAddresses :: forall (r :: Row Type). Contract r (Maybe (Array Address))
getUnusedAddresses = wrapContract QueryM.getUnusedAddresses

getChangeAddress :: forall (r :: Row Type). Contract r (Maybe Address)
getChangeAddress = wrapContract QueryM.getChangeAddress

getRewardAddresses :: forall (r :: Row Type). Contract r (Maybe (Array Address))
getRewardAddresses = wrapContract QueryM.getRewardAddresses

signData :: forall (r::Row Type) . Address -> RawBytes -> Contract r (Maybe RawBytes)
signData address dat = wrapContract (QueryM.signData address dat)

isEnabled :: Wallet -> Aff Boolean
isEnabled  = QueryM.isEnabled

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
