module Examples.Wallet (example) where

import Contract.Prelude

import Contract.Address (getWalletAddresses, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Monad (launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Utxos (getWalletBalance)

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg $ do
    log <<< show =<< getWalletAddresses
    log <<< show =<< getWalletCollateral
    log <<< show =<< getWalletBalance
    liftAff $ publishTestFeedback true
