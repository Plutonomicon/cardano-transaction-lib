-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withWalletSpec
  , module Contract.Address
  , module Serialization
  , module Wallet.Spec
  , module Wallet.Key
  , module Wallet
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (Contract, ContractEnv)
import Control.Monad.Reader (local)
import Data.Lens (Lens, (.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just))
import Effect.Aff.Class(liftAff)
import QueryM(mkWalletBySpec)
import Serialization (privateKeyFromBytes) as Serialization
import Type.Proxy (Proxy(Proxy))
import Wallet (isGeroAvailable, isNamiAvailable) as Wallet
import Wallet (mkKeyWallet, Wallet(KeyWallet))
import Wallet.Spec
  ( WalletSpec(UseKeys, ConnectToNami, ConnectToGero)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )
import Wallet.Key (KeyWallet, privateKeysToKeyWallet) as Wallet
import Wallet.Key (PrivatePaymentKey, PrivateStakeKey)

withWalletSpec
  :: forall (r :: Row Type) (a :: Type)
   . WalletSpec
  -> Contract r a
  -> Contract r a
withWalletSpec walletSpec action = do
  wallet <- liftAff $ mkWalletBySpec walletSpec
  let
    setUpdatedWallet :: ContractEnv r -> ContractEnv r
    setUpdatedWallet =
      simple _Newtype <<< _runtime <<< _wallet .~
        (Just wallet)
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
