-- | Node-only module. Allows to work with Skeys stored in files.
module CTL.Contract.Wallet.KeyFile
  ( mkKeyWalletFromFiles
  , module CTL.Internal.Wallet.KeyFile
  ) where

import Prelude

import CTL.Internal.Wallet (Wallet) as Wallet
import CTL.Internal.Wallet (mkKeyWallet)
import CTL.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privatePaymentKeyFromString
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyFromString
  , privateStakeKeyToFile
  )
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.Path (FilePath)

-- | Load `PrivateKey`s from `skey` files (the files should be in JSON format as
-- | accepted by cardano-cli).
-- | The keys should have `PaymentSigningKeyShelley_ed25519` and
-- | `StakeSigningKeyShelley_ed25519` types, respectively.
-- | The stake key is optional.
-- |
-- | **NodeJS only**
mkKeyWalletFromFiles
  :: FilePath -> Maybe FilePath -> Aff Wallet.Wallet
mkKeyWalletFromFiles paymentKeyFile mbStakeKeyFile = do
  mkKeyWallet
    <$> privatePaymentKeyFromFile paymentKeyFile
    <*> traverse privateStakeKeyFromFile mbStakeKeyFile
