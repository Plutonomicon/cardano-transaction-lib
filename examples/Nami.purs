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
import Contract.Monad
  ( Contract
  , defaultContractConfig
  , launchAff_
  , runContract_
  )

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg $ do
    logAction getWalletAddress
    logAction getWalletCollateral

logAction
  :: forall (a :: Type) (r :: Row Type)
   . Show a
  => Contract r a
  -> Contract r Unit
logAction act = log <<< show =<< act
