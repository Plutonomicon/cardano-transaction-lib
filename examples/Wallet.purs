module Examples.Wallet (example, contract) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Utxos (getWalletBalance, getWalletUtxos)

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

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
