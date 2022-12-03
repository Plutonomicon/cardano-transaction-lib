module Ctl.Examples.Wallet (example, contract) where

import Contract.Prelude

import Contract.Address (getWalletAddresses, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Utxos (getWalletBalance, getWalletUtxos)

contract :: Contract () Unit
contract = do
  log "Address:"
  log <<< show =<< getWalletAddresses
  log "Collateral:"
  log <<< show =<< getWalletCollateral mempty
  log "Balance:"
  log <<< show =<< getWalletBalance
  log "UTxOs:"
  log <<< show =<< getWalletUtxos Nothing Nothing
  log "UTxOs paginated:"
  log <<< show =<< getWalletUtxos Nothing (Just {limit: 10, page: 1})

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
