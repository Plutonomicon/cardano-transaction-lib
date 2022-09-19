module Examples.Wallet (example, contract) where

import Contract.Prelude

import Cardano.Types.Transaction (PublicKey)
import Contract.Address (getWalletAddress, getWalletCollateral)
import Contract.Config (ConfigParams)
import Contract.Log (logDebug')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction (mkPubKey)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import FromData (fromData)
import Partial.Unsafe (unsafePartial)
import ToData (toData)

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
