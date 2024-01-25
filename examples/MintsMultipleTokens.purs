-- | This module demonstrates how the `Contract` interface can be used to build,
-- | balance, and submit a smart-contract transaction. It creates a transaction
-- | that mints a value using three minting policies with different redeemers.
module Ctl.Examples.MintsMultipleTokens
  ( example
  , contract
  , main
  , mintingPolicyRdmrInt1
  , mintingPolicyRdmrInt2
  , mintingPolicyRdmrInt3
  ) where

import Contract.Prelude

import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData(Integer), Redeemer(Redeemer))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  )
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers
  ( mkCurrencySymbol
  , mkTokenName
  ) as Helpers
import Effect.Exception (error)
import JS.BigInt (fromInt) as BigInt

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.MintsMultipleTokens"
  tn1 <- Helpers.mkTokenName "Token with a long name"
  tn2 <- Helpers.mkTokenName "Token"
  mp1 /\ cs1 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt1
  mp2 /\ cs2 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt2
  mp3 /\ cs3 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt3

  let
    constraints :: Constraints.TxConstraints
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

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.mintingPolicy mp1
        <> Lookups.mintingPolicy mp2
        <> Lookups.mintingPolicy mp3

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' $ "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

foreign import redeemer1Script :: String
foreign import redeemer2Script :: String
foreign import redeemer3Script :: String

mintingPolicyRdmrInt1 :: Contract MintingPolicy
mintingPolicyRdmrInt1 = do
  liftMaybe (error "Error decoding redeemer1Script") do
    envelope <- decodeTextEnvelope redeemer1Script
    PlutusMintingPolicy <$> plutusScriptV1FromEnvelope envelope

mintingPolicyRdmrInt2 :: Contract MintingPolicy
mintingPolicyRdmrInt2 = do
  liftMaybe (error "Error decoding redeemer2Script") do
    envelope <- decodeTextEnvelope redeemer2Script
    PlutusMintingPolicy <$> plutusScriptV1FromEnvelope envelope

mintingPolicyRdmrInt3 :: Contract MintingPolicy
mintingPolicyRdmrInt3 = do
  liftMaybe (error "Error decoding redeemer3Script") do
    envelope <- decodeTextEnvelope redeemer3Script
    PlutusMintingPolicy <$> plutusScriptV1FromEnvelope envelope
