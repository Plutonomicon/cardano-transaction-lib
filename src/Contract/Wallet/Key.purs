-- | `KeyWallet` type and its utility functions.
module Contract.Wallet.Key
  ( module X
  , publicKeyFromPrivateKey
  ) where

import Cardano.Types (PrivateKey, PublicKey)
import Cardano.Types.PrivateKey as PrivateKey
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , getPrivatePaymentKey
  , getPrivateStakeKey
  , privateKeysToKeyWallet
  ) as X
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , StakeKeyPresence(WithStakeKey, WithoutStakeKey)
  , mkKeyWalletFromMnemonic
  ) as X
import Prim.TypeError (class Warn, Text)

publicKeyFromPrivateKey
  :: Warn (Text "Deprecated: use Cardano.Types.PrivateKey.toPublicKey")
  => PrivateKey
  -> PublicKey
publicKeyFromPrivateKey = PrivateKey.toPublicKey
