-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module Examples.Pkh2Pkh (main, contract, example) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Test.E2E (publishTestFeedback)
import Contract.Transaction (awaitTxConfirmedWithTimeout)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
import Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.Pkh2Pkh"
  pkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  skh <- liftedM "Failed to get own SKH" ownStakePubKeyHash

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToPubKeyAddress pkh skh
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmedWithTimeout (wrap 100.0) txId
  logInfo' $ "Tx submitted successfully!"
  liftAff $ publishTestFeedback true

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
