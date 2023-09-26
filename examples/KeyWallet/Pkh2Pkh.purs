-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a transaction. It creates a simple transaction that gets
-- | UTxOs from the user's wallet and sends the selected amount to the specified
-- | address
module Ctl.Examples.KeyWallet.Pkh2Pkh (main) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.Pkh2Pkh"

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustPayToPubKey pkh $
      Value.lovelaceValueOf lovelace

    lookups :: Lookups.ScriptLookups
    lookups = mempty

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  liftEffect unlock
  logInfo' $ "Tx submitted successfully!"
