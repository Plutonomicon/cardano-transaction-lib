-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module Examples.PlutusV2.AlwaysSucceeds
  ( main
  , example
  , contract
  , alwaysSucceedsScriptV2
  ) where

import Contract.Prelude

import Examples.AlwaysSucceeds
  ( payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
  )

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , launchAff_
  , runContract
  )
import Contract.Scripts (Validator, validatorHash)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , plutusV2Script
  )

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.PlutusV2.AlwaysSucceeds"
  validator <- alwaysSucceedsScriptV2
  let vhash = validatorHash validator
  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceeds vhash
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash validator txId

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

foreign import alwaysSucceeds :: String

alwaysSucceedsScriptV2 :: Contract () Validator
alwaysSucceedsScriptV2 = wrap <<< plutusV2Script <$> textEnvelopeBytes
  alwaysSucceeds
  PlutusScriptV2
