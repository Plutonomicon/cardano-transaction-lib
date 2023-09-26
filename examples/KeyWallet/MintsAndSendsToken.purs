-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a token using the `AlwaysMints` policy and sends it along with
-- | the selected amount to the specified address.
module Ctl.Examples.KeyWallet.MintsAndSendsToken (main) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Examples.AlwaysMints (alwaysMintsPolicy)
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  ) as Helpers
import Ctl.Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)

main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> do
  logInfo' "Running Examples.KeyWallet.MintsAndSendsToken"

  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "TheToken"

  let
    constraints :: Constraints.TxConstraints
    constraints = mconcat
      [ Constraints.mustMintValue (Value.singleton cs tn one)
      , Constraints.mustPayToPubKey pkh
          (Value.lovelaceValueOf lovelace <> Value.singleton cs tn one)
      ]

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  liftEffect unlock
