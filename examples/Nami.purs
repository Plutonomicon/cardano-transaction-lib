-- Examples of how to use the Nami wallet interface through `Contract`
--
-- To run: `npm run dev` and visit `localhost:4008` in your browser. Make sure
-- that you have the Nami browser extension installed. Allow the page to access
-- your wallet when prompted by Nami
--
-- NOTE: due to Nami's limitations, only chromium-based browsers are supported

module Examples.Nami (main) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (testnetNamiConfig)
import Contract.Monad (launchAff_, runContract)
import Contract.Utxos (getWalletBalance)

main :: Effect Unit
main = launchAff_ do
  runContract testnetNamiConfig do
    log <<< show =<< getWalletAddress
    log <<< show =<< getWalletCollateral
    log <<< show =<< getWalletBalance
