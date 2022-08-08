-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends two Ada back to the same wallet address
module Examples.Pkh2Pkh (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash, ownStakePubKeyHash)
import Contract.Config (testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftedE, liftedM, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( balanceAndSignTxM
  , awaitTxConfirmedWithTimeout
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
import Contract.Test.E2E (publishTestFeedback)

main :: Effect Unit
main = launchAff_ do
  runContract testnetNamiConfig do
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

    ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    bsTx <-
      liftedM "Failed to balance/sign tx" $ balanceAndSignTxM ubTx
    txId <- submit bsTx
    logInfo' $ "Tx ID: " <> show txId
    awaitTxConfirmedWithTimeout (wrap 100.0) txId
    logInfo' $ "Tx submitted successfully!"
    liftAff $ publishTestFeedback true
