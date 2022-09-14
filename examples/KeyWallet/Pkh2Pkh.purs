-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends the selected amount to the specified
-- | address
module CTL.Examples.KeyWallet.Pkh2Pkh (main) where

import CTL.Contract.Prelude

import CTL.Contract.Log (logInfo')
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Transaction (awaitTxConfirmed)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value (lovelaceValueOf) as Value
import CTL.Examples.Helpers (buildBalanceSignAndSubmitTx) as Helpers
import CTL.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.Pkh2Pkh"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToPubKey pkh $
      Value.lovelaceValueOf lovelace

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmed txId
  liftEffect unlock
  logInfo' $ "Tx submitted successfully!"
