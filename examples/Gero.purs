-- Follow instructions on running examples to see it in action.
-- Make sure that you have the Gero browser extension installed. Allow the page
-- to access your wallet when prompted by Gero
--
-- NOTE: Gero has the same limitations as Nami, i.e., Chromium-based only
module Examples.Gero (main) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (testnetGeroConfig)
import Contract.Monad (launchAff_, runContract)
import Contract.Utxos (getWalletBalance)

main :: Effect Unit
main = launchAff_ $ do
  runContract testnetGeroConfig $ do
    log <<< show =<< getWalletAddress
    log <<< show =<< getWalletCollateral
    log <<< show =<< getWalletBalance
