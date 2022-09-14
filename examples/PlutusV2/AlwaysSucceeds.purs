-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that pays two Ada to the `AlwaysSucceeds` script address
module CTL.Examples.PlutusV2.AlwaysSucceeds
  ( main
  , example
  , contract
  , alwaysSucceedsScriptV2
  ) where

import CTL.Contract.Prelude

import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad
  ( Contract
  , launchAff_
  , runContract
  )
import CTL.Contract.Scripts (Validator, validatorHash)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV2)
  , textEnvelopeBytes
  )
import CTL.Contract.Transaction
  ( awaitTxConfirmed
  , plutusV2Script
  )
import CTL.Examples.AlwaysSucceeds
  ( payToAlwaysSucceeds
  , spendFromAlwaysSucceeds
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
  publishTestFeedback true

foreign import alwaysSucceeds :: String

alwaysSucceedsScriptV2 :: Contract () Validator
alwaysSucceedsScriptV2 = wrap <<< plutusV2Script <$> textEnvelopeBytes
  alwaysSucceeds
  PlutusScriptV2
