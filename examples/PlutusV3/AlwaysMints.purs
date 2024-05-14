-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module Ctl.Examples.PlutusV3.AlwaysMints (main, example, contract) where

import Contract.Prelude

import Contract.TxConstraints as Constraints
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.ScriptLookups as Lookups
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.Value as Value
import Ctl.Examples.PlutusV3.Scripts.AlwaysMints (alwaysMintsPolicyV3)
import Ctl.Examples.Helpers (mkCurrencySymbol, mkTokenName) as Helpers
import JS.BigInt as BigInt

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV3.AlwaysMints"
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicyV3
  tn <- Helpers.mkTokenName "TheToken"
  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustMintValue
      $ Value.singleton cs tn
      $ BigInt.fromInt 100

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.mintingPolicy mp
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

