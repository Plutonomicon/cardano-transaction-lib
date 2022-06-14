-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromFile
  , mkKeyWalletFromPrivateKey
  , module ContractAddress
  , module Wallet
  ) where

import Prelude

import Contract.Address (getWalletAddress, getWalletCollateral) as ContractAddress
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

import Node.Path (FilePath)
import Serialization.Types (PrivateKey)
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami)
  , mkNamiWalletAff
  , mkGeroWalletAff
  ) as Wallet
import Wallet (mkKeyWallet)
import Wallet.Key (privateKeyFromFile)
import Wallet.Key (privateKeyFromNormalBytes, privateKeyToKeyWallet) as Wallet

-- | Load PrivateKey from a skey file (the file should be in JSON format as
-- | accepted by cardano-cli. The JSON should have
-- | `"type": "PaymentSigningKeyShelley_ed25519"` field)
mkKeyWalletFromFile
  :: FilePath -> Aff (Maybe Wallet.Wallet)
mkKeyWalletFromFile filePath = do
  map mkKeyWallet <$> privateKeyFromFile filePath

mkKeyWalletFromPrivateKey :: PrivateKey -> Wallet.Wallet
mkKeyWalletFromPrivateKey = mkKeyWallet
