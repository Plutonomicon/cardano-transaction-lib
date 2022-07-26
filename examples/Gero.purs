-- Follow instructions on running examples to see it in action.
-- Make sure that you have the Gero browser extension installed. Allow the page
-- to access your wallet when prompted by Gero
--
-- NOTE: Gero has the same limitations as Nami, i.e., Chromium-based only
module Examples.Gero (main) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Monad (configWithLogLevel, launchAff_, runContract_)
import Contract.Utxos (getWalletBalance)
import Contract.Wallet (mkGeroWalletAff)
import Data.Log.Level (LogLevel(Trace))
import Serialization.Address (NetworkId(TestnetId))
import Contract.Test.E2E (publishTestFeedback)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- mkGeroWalletAff
  cfg <- configWithLogLevel TestnetId wallet Trace
  runContract_ cfg $ do
    log <<< show =<< getWalletAddress
    log <<< show =<< getWalletCollateral
    log <<< show =<< getWalletBalance
    liftAff $ publishTestFeedback true
