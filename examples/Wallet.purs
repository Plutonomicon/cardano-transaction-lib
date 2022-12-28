module Ctl.Examples.Wallet (example, contract) where

import Contract.Prelude

import Contract.Address (getWalletAddresses, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value as Value
import Data.BigInt as BigInt

contract :: Contract () Unit
contract = do
  log "Address:"
  log <<< show =<< getWalletAddresses
  log "Collateral:"
  log <<< show =<< getWalletCollateral
    (Value.valueToCoin $ Value.lovelaceValueOf $ BigInt.fromInt 5_000_000)
  log "Balance:"
  log <<< show =<< getWalletBalance
  log "UTxOs:"
  log <<< show =<< getWalletUtxos Nothing Nothing
  log "UTxOs paginated:"
  log <<< show =<< getWalletUtxos Nothing (Just { limit: 10, page: 0 })

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract
