module Examples.Wallet (example, contract) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Utxos (getWalletBalance, getWalletUtxos)

contract :: Contract () Unit
contract = do
  log <<< show =<< getWalletAddress
  log <<< show =<< getWalletCollateral
  log <<< show =<< getWalletBalance
  liftAff $ publishTestFeedback true

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
