-- | Node-only module. Allows to work with Skeys stored in files.
module Contract.Wallet.KeyFile
  ( mkKeyWalletFromFile
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Wallet (Wallet) as Wallet
import Wallet (mkKeyWallet)
import Wallet.KeyFile (privateKeyFromFile)

-- | Load PrivateKey from a skey file (the file should be in JSON format as
-- | accepted by cardano-cli. The JSON should have
-- | `"type": "PaymentSigningKeyShelley_ed25519"` field)
-- |
-- | **NodeJS only**
mkKeyWalletFromFile
  :: FilePath -> Aff (Maybe Wallet.Wallet)
mkKeyWalletFromFile filePath = do
  map mkKeyWallet <$> privateKeyFromFile filePath
