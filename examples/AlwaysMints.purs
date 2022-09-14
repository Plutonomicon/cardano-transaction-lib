-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module CTL.Examples.AlwaysMints (main, example, contract, alwaysMintsPolicy) where

import CTL.Contract.Prelude

import CTL.Contract.Config (ConfigParams, testnetNamiConfig)
import CTL.Contract.Log (logInfo')
import CTL.Contract.Monad (Contract, launchAff_, runContract)
import CTL.Contract.ScriptLookups as Lookups
import CTL.Contract.Scripts (MintingPolicy)
import CTL.Contract.Test.E2E (publishTestFeedback)
import CTL.Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import CTL.Contract.Transaction (awaitTxConfirmed, plutusV1Script)
import CTL.Contract.TxConstraints as Constraints
import CTL.Contract.Value as Value
import Data.BigInt as BigInt
import CTL.Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  ) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract () Unit
contract = do
  logInfo' "Running Examples.AlwaysMints"
  mp /\ cs <- Helpers.mkCurrencySymbol alwaysMintsPolicy
  tn <- Helpers.mkTokenName "TheToken"
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValue
      $ Value.singleton cs tn
      $ BigInt.fromInt 100

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ $ do
  runContract cfg contract

  publishTestFeedback true

foreign import alwaysMints :: String

alwaysMintsPolicy :: Contract () MintingPolicy
alwaysMintsPolicy = wrap <<< plutusV1Script <$> textEnvelopeBytes alwaysMints
  PlutusScriptV1
