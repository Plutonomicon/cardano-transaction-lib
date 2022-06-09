-- Follow instructions on running examples to see it in action.
-- Make sure that you have the Gero browser extension installed. Allow the page
-- to access your wallet when prompted by Gero
--
-- NOTE: Gero has the same limitations as Nami, i.e., Chromium-based only
module Examples.Gero (main) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad
  ( ContractConfig(ContractConfig)
  , launchAff_
  , runContract_
  , traceContractConfig
  )
import Contract.Wallet (mkGeroWalletAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkGeroWalletAff
  cfg <- over ContractConfig _ { wallet = wallet } <$> traceContractConfig
  runContract_ cfg $ do
    log <<< show =<< getWalletAddress
    log <<< show =<< getWalletCollateral
