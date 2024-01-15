-- | `KeyWallet` type and its utility functions.
module Contract.Wallet.Key
  ( module X
  , publicKeyFromPrivateKey
  ) where

import Prelude

import Cardano.Serialization.Lib (privateKey_toPublic)
import Ctl.Internal.Cardano.Types.Transaction (PrivateKey, PublicKey)
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  , privateKeysToKeyWallet
  ) as X
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , mkKeyWalletFromMnemonic
  ) as X
import Data.Newtype (unwrap, wrap)

publicKeyFromPrivateKey :: PrivateKey -> PublicKey
publicKeyFromPrivateKey = wrap <<< privateKey_toPublic <<< unwrap
