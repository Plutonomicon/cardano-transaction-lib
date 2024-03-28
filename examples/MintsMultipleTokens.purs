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

import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Contract.Config (ContractParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData(Integer), RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers (mkAssetName, mkCurrencySymbol) as Helpers
import Effect.Exception (error)
import JS.BigInt (fromInt) as BigInt
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = example testnetNamiConfig

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.MintsMultipleTokens"
  tn1 <- Helpers.mkAssetName "Token with a long name"
  tn2 <- Helpers.mkAssetName "Token"
  mp1 /\ cs1 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt1
  mp2 /\ cs2 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt2
  mp3 /\ cs3 <- Helpers.mkCurrencySymbol mintingPolicyRdmrInt3

  let
    constraints :: Constraints.TxConstraints
    constraints = unsafePartial $ mconcat
      [ Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ Integer (BigInt.fromInt 1))
          (Mint.singleton cs1 tn1 Int.one <> Mint.singleton cs1 tn2 Int.one)
      , Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ Integer (BigInt.fromInt 2))
          (Mint.singleton cs2 tn1 Int.one <> Mint.singleton cs2 tn2 Int.one)
      , Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ Integer (BigInt.fromInt 3))
          (Mint.singleton cs3 tn1 Int.one <> Mint.singleton cs3 tn2 Int.one)
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
    PlutusMintingPolicy <$> plutusScriptFromEnvelope envelope

mintingPolicyRdmrInt2 :: Contract MintingPolicy
mintingPolicyRdmrInt2 = do
  liftMaybe (error "Error decoding redeemer2Script") do
    envelope <- decodeTextEnvelope redeemer2Script
    PlutusMintingPolicy <$> plutusScriptFromEnvelope envelope

mintingPolicyRdmrInt3 :: Contract MintingPolicy
mintingPolicyRdmrInt3 = do
  liftMaybe (error "Error decoding redeemer3Script") do
    envelope <- decodeTextEnvelope redeemer3Script
    PlutusMintingPolicy <$> plutusScriptFromEnvelope envelope
