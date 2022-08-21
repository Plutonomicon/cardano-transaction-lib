-- Follow instructions on running examples to see it in action.
-- Make sure that you have the Eternl browser extension installed. Allow the page
-- to access your wallet when prompted by Eternl
-- Ensure that your Eternl account is "activated" and can interface with dApps.
-- To do so navigate to your desired wallet > "Account List" tab, and click "Activate" on the account you wish to use.
--
-- NOTE: Eternl has the same limitations as Nami, i.e., Chromium-based only

module Examples.Eternl (main) where

import Contract.Prelude

import Contract.Address (getWalletAddresses, getWalletCollateral)
import Contract.Config (testnetEternlConfig)
import Contract.Monad (launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Utxos (getWalletBalance)

main :: Effect Unit
main = launchAff_ do
  runContract testnetEternlConfig do
    log <<< show =<< getWalletAddresses
    log <<< show =<< getWalletCollateral
    log <<< show =<< getWalletBalance
  publishTestFeedback true
