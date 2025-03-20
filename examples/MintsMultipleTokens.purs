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

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(MintAsset)
  )
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.Transaction as Transaction
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.PlutusData (PlutusData(Integer), RedeemerDatum(RedeemerDatum))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromBuildPlan)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt (fromInt) as BigInt

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: false }
  }

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.MintsMultipleTokens"
  tn1 <- Helpers.mkAssetName "Token with a long name"
  tn2 <- Helpers.mkAssetName "Token"
  mp1 <- mintingPolicyRdmrInt1
  mp2 <- mintingPolicyRdmrInt2
  mp3 <- mintingPolicyRdmrInt3
  let
    cs1 = PlutusScript.hash mp1
    cs2 = PlutusScript.hash mp2
    cs3 = PlutusScript.hash mp3

  let
    plan =
      [ MintAsset cs1 tn1 Int.one
          ( PlutusScriptCredential (ScriptValue mp1) $ RedeemerDatum $ Integer
              (BigInt.fromInt 1)
          )
      , MintAsset cs1 tn2 Int.one
          ( PlutusScriptCredential (ScriptValue mp1) $ RedeemerDatum $ Integer
              (BigInt.fromInt 1)
          )
      , MintAsset cs2 tn2 Int.one
          ( PlutusScriptCredential (ScriptValue mp2) $ RedeemerDatum $ Integer
              (BigInt.fromInt 2)
          )
      , MintAsset cs3 tn2 Int.one
          ( PlutusScriptCredential (ScriptValue mp3) $ RedeemerDatum $ Integer
              (BigInt.fromInt 3)
          )
      ]

  tx <- submitTxFromBuildPlan Map.empty mempty plan
  awaitTxConfirmed $ Transaction.hash tx
  logInfo' $ "Tx submitted successfully!"

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

foreign import redeemer1Script :: String
foreign import redeemer2Script :: String
foreign import redeemer3Script :: String

mintingPolicyRdmrInt1 :: Contract PlutusScript
mintingPolicyRdmrInt1 = do
  liftMaybe (error "Error decoding redeemer1Script") do
    envelope <- decodeTextEnvelope redeemer1Script
    plutusScriptFromEnvelope envelope

mintingPolicyRdmrInt2 :: Contract PlutusScript
mintingPolicyRdmrInt2 = do
  liftMaybe (error "Error decoding redeemer2Script") do
    envelope <- decodeTextEnvelope redeemer2Script
    plutusScriptFromEnvelope envelope

mintingPolicyRdmrInt3 :: Contract PlutusScript
mintingPolicyRdmrInt3 = do
  liftMaybe (error "Error decoding redeemer3Script") do
    envelope <- decodeTextEnvelope redeemer3Script
    plutusScriptFromEnvelope envelope
