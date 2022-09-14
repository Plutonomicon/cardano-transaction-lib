-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module CTL.Examples.Pkh2Pkh (main, contract, example) where

import CTL.Contract.Prelude

import CTL.Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, launchAff_, liftedM, runContract)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.Transaction (awaitTxConfirmedWithTimeout)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value as Value
import CTL.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import Data.BigInt as BigInt

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
