-- Examples of how to use the Gero wallet interface through `Contract`
--
-- To run: `npm run dev` and visit `localhost:4008` in your browser. Make sure
-- that you have the Gero browser extension installed. Allow the page to access
-- your wallet when prompted by Gero
--
-- NOTE: Gero has the same limitations as Nami, i.e., Chromium-based only

module Examples.Gero (main) where

import Contract.Prelude
import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad
  ( Contract
  , defaultContractConfig
  , launchAff_
  , runContract_
  )

import Effect.Class (liftEffect)
import Wallet (mkGeroWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig mkGeroWalletAff
  runContract_ cfg $ do
    logAction getWalletAddress
    logAction getWalletCollateral

logAction
  :: forall (a :: Type) (r :: Row Type)
   . Show a
  => Contract r a
  -> Contract r Unit
logAction act = log <<< show =<< act
