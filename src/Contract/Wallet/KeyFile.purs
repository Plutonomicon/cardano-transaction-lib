-- | Node-only module. Allows working with Skeys stored in files.
module Contract.Wallet.KeyFile
  ( mkKeyWalletFromFiles
  , mkKeyWalletFromFilesAff
  , module Wallet.KeyFile
  ) where

import Prelude

import Contract.Config (NetworkId)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Contract.Monad (Contract)
import Ctl.Internal.Wallet.Key (KeyWallet, privateKeysToKeyWallet)
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
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Node.Path (FilePath)

-- | Load `PrivateKey`s from `skey` files (the files should be in JSON format as
-- | accepted by cardano-cli).
-- | The keys should have `PaymentSigningKeyShelley_ed25519` and
-- | `StakeSigningKeyShelley_ed25519` types, respectively.
-- | The stake key is optional.
-- |
-- | **NodeJS only**
mkKeyWalletFromFiles
  :: FilePath -> Maybe FilePath -> Contract KeyWallet
mkKeyWalletFromFiles paymentKeyFile mbStakeKeyFile = do
  networkId <- asks _.networkId
  liftAff $ mkKeyWalletFromFilesAff networkId paymentKeyFile mbStakeKeyFile

-- | Load `PrivateKey`s from `skey` files (the files should be in JSON format as
-- | accepted by cardano-cli) given a network id.
-- | The keys should have `PaymentSigningKeyShelley_ed25519` and
-- | `StakeSigningKeyShelley_ed25519` types, respectively.
-- | The stake key is optional.
-- |
-- | **NodeJS only**
mkKeyWalletFromFilesAff
  :: NetworkId -> FilePath -> Maybe FilePath -> Aff KeyWallet
mkKeyWalletFromFilesAff networkId paymentKeyFile mbStakeKeyFile =
  privateKeysToKeyWallet networkId
    <$> privatePaymentKeyFromFile paymentKeyFile
    <*> traverse privateStakeKeyFromFile mbStakeKeyFile
