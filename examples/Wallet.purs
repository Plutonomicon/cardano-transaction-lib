module CTL.Examples.Wallet (example, contract) where

import CTL.Contract.Prelude

import CTL.Contract.Address (getWalletAddress, getWalletCollateral)
import CTL.Contract.Config (ConfigParams)
import CTL.Contract.Monad (Contract, launchAff_, runContract)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.Utxos (getWalletBalance, getWalletUtxos)

contract :: Contract () Unit
contract = do
  log "Address:"
  log <<< show =<< getWalletAddress
  log "Collateral:"
  log <<< show =<< getWalletCollateral
  log "Balance:"
  log <<< show =<< getWalletBalance
  log "UTxOs:"
  log <<< show =<< getWalletUtxos
  liftAff $ publishTestFeedback true

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
