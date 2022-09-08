-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using the `AlwaysMints` policy
module Examples.AlwaysMints (main, example, contract, alwaysMintsPolicy) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Test.E2E (publishTestFeedback)
import Contract.TextEnvelope
  ( TextEnvelopeType(PlutusScriptV1)
  , textEnvelopeBytes
  )
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
import Examples.Helpers
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
alwaysMintsPolicy = wrap <<< wrap <$> textEnvelopeBytes alwaysMints
  PlutusScriptV1
