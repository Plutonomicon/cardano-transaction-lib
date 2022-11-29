-- | Node-only module. Allows to work with Skeys stored in files.
module Contract.Wallet.KeyFile
  ( mkKeyWalletFromFiles
  , module Ctl.Internal.Wallet.KeyFile
  ) where

import Prelude

import Control.Monad.Reader.Class (asks)
import Effect.Aff.Class (liftAff)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Wallet.Key (KeyWallet) as Wallet
import Ctl.Internal.Wallet.Key (privateKeysToKeyWallet)
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privatePaymentKeyFromTextEnvelope
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyFromTextEnvelope
  , privateStakeKeyToFile
  )
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Node.Path (FilePath)

-- | Load `PrivateKey`s from `skey` files (the files should be in JSON format as
-- | accepted by cardano-cli).
-- | The keys should have `PaymentSigningKeyShelley_ed25519` and
-- | `StakeSigningKeyShelley_ed25519` types, respectively.
-- | The stake key is optional.
-- |
-- | **NodeJS only**
mkKeyWalletFromFiles
  :: FilePath -> Maybe FilePath -> Contract Wallet.KeyWallet
mkKeyWalletFromFiles paymentKeyFile mbStakeKeyFile = do
  networkId <- asks _.networkId
  liftAff $ privateKeysToKeyWallet networkId
    <$> privatePaymentKeyFromFile paymentKeyFile
    <*> traverse privateStakeKeyFromFile mbStakeKeyFile
