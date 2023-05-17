-- | `KeyWallet` type and its utility functions.
module Contract.Wallet.Key
  ( module X
  , publicKeyFromPrivateKey
  , mkKeyWalletFromMnemonic
  ) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction (PublicKey, mkFromCslPubKey)
import Ctl.Internal.Serialization.Keys (publicKeyFromPrivateKey) as Internal
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Wallet.Bip32
  ( bip32ToPrivateKey
  , cip1852AccountFromMnemonic
  , derivePaymentKey
  , deriveStakeKey
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  )
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , privateKeysToKeyWallet
  ) as X
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.UInt (UInt)

publicKeyFromPrivateKey :: PrivateKey -> PublicKey
publicKeyFromPrivateKey = mkFromCslPubKey <<< Internal.publicKeyFromPrivateKey

-- | Create a wallet given a mnemonic phrase and account index
mkKeyWalletFromMnemonic :: String -> UInt -> Either String KeyWallet
mkKeyWalletFromMnemonic phrase accountIndex = do
  account <- cip1852AccountFromMnemonic phrase accountIndex
  let
    paymentKey = derivePaymentKey account zero # bip32ToPrivateKey
    stakeKey = deriveStakeKey account # bip32ToPrivateKey
  pure $
    privateKeysToKeyWallet
      (PrivatePaymentKey paymentKey)
      (Just $ PrivateStakeKey stakeKey)
