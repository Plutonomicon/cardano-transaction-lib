-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a token using the `AlwaysMints` policy and sends it along with
-- | the selected amount to the specified address.
module CTL.Examples.KeyWallet.MintsAndSendsToken (main) where

import CTL.Contract.Prelude

import CTL.Contract.Log (logInfo')
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Transaction (awaitTxConfirmed)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value as Value
import CTL.Examples.AlwaysMints (alwaysMintsPolicy)
import CTL.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  ) as Helpers
import CTL.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.MintsAndSendsToken"

  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "TheToken"

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Constraints.mustMintValue (Value.singleton cs tn one)
      , Constraints.mustPayToPubKey pkh
          (Value.lovelaceValueOf lovelace <> Value.singleton cs tn one)
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints
  awaitTxConfirmed txId
  liftEffect unlock
