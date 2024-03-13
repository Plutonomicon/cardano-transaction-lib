-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Ctl.Examples.PlutusV3.AlwaysSucceeds (main, example, contract) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Scripts (validatorHash)
import Contract.Transaction (awaitTxConfirmed)
import Ctl.Examples.AlwaysSucceeds
  ( payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  )
import Ctl.Examples.PlutusV3.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV3)

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV3.AlwaysSucceeds"
  validator <- alwaysSucceedsScriptV3
  let vhash = validatorHash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

