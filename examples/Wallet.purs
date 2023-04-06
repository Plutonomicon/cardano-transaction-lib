module Ctl.Examples.Wallet (example, contract) where

import Contract.Prelude

import Contract.Config (ContractParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , getWalletCollateral
  , getWalletUtxos
  )

contract :: Contract Unit
contract = do
  log "Address:"
  log <<< show =<< getWalletAddresses
  log "Collateral:"
  log <<< show =<< getWalletCollateral
  log "Balance:"
  log <<< show =<< getWalletBalance
  log "UTxOs:"
  log <<< show =<< getWalletUtxos

example :: ContractParams -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
