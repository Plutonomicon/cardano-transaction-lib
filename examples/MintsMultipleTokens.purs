-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using three minting policies with different redeemers.
module Examples.MintsMultipleTokens
  ( example
  , main
  , mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData(Integer), Redeemer(Redeemer))
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
import Data.BigInt (fromInt) as BigInt
import Examples.Helpers
  ( buildBalanceSignAndSubmitTx
  , mkCurrencySymbol
  , mkTokenName
  ) as Helpers

main :: Effect Unit
main = example testnetNamiConfig

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.MintsMultipleTokens"
    tn1 <- Helpers.mkTokenName "Token with a long name"
    tn2 <- Helpers.mkTokenName "Token"
    mp1 /\ cs1 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt1
    mp2 /\ cs2 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt2
    mp3 /\ cs3 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt3

    let
      constraints :: Constraints.TxConstraints Void Void
      constraints = mconcat
        [ Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 1))
            (Value.singleton cs1 tn1 one <> Value.singleton cs1 tn2 one)
        , Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 2))
            (Value.singleton cs2 tn1 one <> Value.singleton cs2 tn2 one)
        , Constraints.mustMintValueWithRedeemer
            (Redeemer $ Integer (BigInt.fromInt 3))
            (Value.singleton cs3 tn1 one <> Value.singleton cs3 tn2 one)
        ]

      lookups :: Lookups.ScriptLookups Void
      lookups =
        Lookups.mintingPolicy mp1
          <> Lookups.mintingPolicy mp2
          <> Lookups.mintingPolicy mp3

    txId <- Helpers.buildBalanceSignAndSubmitTx lookups constraints

    awaitTxConfirmed txId
    logInfo' $ "Tx submitted successfully!"

  publishTestFeedback true

foreign import redeemerInt1 :: String
foreign import redeemerInt2 :: String
foreign import redeemerInt3 :: String

mintingPolicyRdmrInt1 :: Contract () MintingPolicy
mintingPolicyRdmrInt1 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt1
  PlutusScriptV1

mintingPolicyRdmrInt2 :: Contract () MintingPolicy
mintingPolicyRdmrInt2 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt2
  PlutusScriptV1

mintingPolicyRdmrInt3 :: Contract () MintingPolicy
mintingPolicyRdmrInt3 = wrap <<< wrap <$> textEnvelopeBytes redeemerInt3
  PlutusScriptV1
