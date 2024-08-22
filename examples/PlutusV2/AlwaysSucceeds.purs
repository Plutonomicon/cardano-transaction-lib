-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Ctl.Examples.PlutusV2.AlwaysSucceeds
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Scripts (validatorHash)
import Contract.Transaction (awaitTxConfirmed)
import Ctl.Examples.AlwaysSucceeds
  ( payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  )
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV2.AlwaysSucceeds"
  validator <- alwaysSucceedsScriptV2
  let vhash = validatorHash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract
