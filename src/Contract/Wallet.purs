-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , getNetworkId
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , getWallet
  , signData
  , module Contract.Address
  , module Contract.Utxos
  , module Deserialization.Keys
  , module Wallet
  , module Ctl.Internal.Wallet.Key
  , module Ctl.Internal.Wallet.KeyFile
  , module Ctl.Internal.Wallet.Spec
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (Contract, ContractEnv)
import Contract.Utxos (getWalletUtxos) as Contract.Utxos
import Control.Monad.Reader (local)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes) as Deserialization.Keys
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , signData
  ) as Contract
import Ctl.Internal.Serialization.Address (Address, NetworkId)
import Ctl.Internal.Types.RawBytes (RawBytes)
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
import Ctl.Internal.Wallet (Wallet(KeyWallet), mkKeyWallet)
import Ctl.Internal.Wallet.Cip30 (DataSignature)
import Ctl.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Ctl.Internal.Wallet.Key
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
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
      )
  )
import Data.Lens (Lens, (.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just))
import Type.Proxy (Proxy(Proxy))

getNetworkId :: Contract NetworkId
getNetworkId = asks _.networkId

getUnusedAddresses :: Contract (Array Address)
getUnusedAddresses = Contract.getUnusedAddresses

getChangeAddress :: Contract (Maybe Address)
getChangeAddress = Contract.getChangeAddress

getRewardAddresses :: Contract (Array Address)
getRewardAddresses = Contract.getRewardAddresses

signData
  :: Address
  -> RawBytes
  -> Contract (Maybe DataSignature)
signData address dat = Contract.signData address dat

getWallet :: Contract (Maybe Wallet)
getWallet = Contract.getWallet

withKeyWallet
  :: forall (a :: Type)
   . Wallet.KeyWallet
  -> Contract a
  -> Contract a
withKeyWallet wallet action = do
  local _ { wallet = Just $ KeyWallet wallet } action

mkKeyWalletFromPrivateKeys
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> Wallet
mkKeyWalletFromPrivateKeys = mkKeyWallet
