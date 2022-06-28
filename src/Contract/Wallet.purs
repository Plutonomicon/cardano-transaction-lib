-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKey
  , getWalletBalance
  , module ContractAddress
  , module Wallet
  , module Serialization
  ) where

import Cardano.Types.Value (Value)
import Contract.Address (getWalletAddress, getWalletCollateral) as ContractAddress
import Contract.Monad (Contract, wrapContract)
import Data.Maybe (Maybe)
import QueryM.Utxos as Utxos
import Serialization (privateKeyFromBytes) as Serialization
import Serialization.Types (PrivateKey)
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami)
  , mkNamiWalletAff
  , mkGeroWalletAff
  ) as Wallet
import Wallet (mkKeyWallet)
import Wallet.Key (KeyWallet, privateKeyToKeyWallet) as Wallet

mkKeyWalletFromPrivateKey :: PrivateKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet

getWalletBalance
  :: forall (r :: Row Type)
   . Contract r (Maybe Value)
getWalletBalance = wrapContract Utxos.getWalletBalance
