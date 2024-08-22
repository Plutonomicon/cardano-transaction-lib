-- | Node-only module. Allows working with Skeys stored in files.
module Contract.Wallet.KeyFile
  ( mkKeyWalletFromFiles
  , module Wallet.KeyFile
  ) where

import Prelude

import Cardano.Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyFromTextEnvelope
  , privateStakeKeyToFile
  ) as Wallet.KeyFile
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privateStakeKeyFromFile
  )
import Data.Maybe (Maybe(Nothing))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.Path (FilePath)

-- | Load `PrivateKey`s from `skey` files (the files should be in JSON format as
-- | accepted by cardano-cli) given a network id.
-- | The keys should have `PaymentSigningKeyShelley_ed25519` and
-- | `StakeSigningKeyShelley_ed25519` types, respectively.
-- | The stake key is optional.
-- |
-- | **NodeJS only**
mkKeyWalletFromFiles
  :: FilePath -> Maybe FilePath -> Aff KeyWallet
mkKeyWalletFromFiles paymentKeyFile mbStakeKeyFile =
  privateKeysToKeyWallet
    <$> privatePaymentKeyFromFile paymentKeyFile
    <*> traverse privateStakeKeyFromFile mbStakeKeyFile
    -- FIXME: allow to provide drep key
    <*> pure Nothing
