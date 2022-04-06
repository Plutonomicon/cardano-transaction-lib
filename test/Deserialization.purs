module Test.Deserialization (suite) where

import Prelude

import Data.Array as Array
import Data.BigInt as BigInt
import Data.Maybe (isJust, isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, expectError)
import TestM (TestPlanM)
import Untagged.Union (asOneOf)

import Deserialization.BigNum (bigNumToBigInt)
import Deserialization.BigInt as DB
import Deserialization.FromBytes (fromBytes)
import Deserialization.NativeScript as NSD
import Deserialization.PlutusData as DPD
import Deserialization.UnspentOutput (convertUnspentOutput, mkTransactionUnspentOutput, newTransactionUnspentOutputFromBytes)
import Deserialization.WitnessSet (deserializeWitnessSet, convertWitnessSet)
import Serialization as Serialization
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.BigInt as SB
import Serialization.NativeScript (convertNativeScript) as NSS
import Serialization.PlutusData as SPD
import Serialization.Types (TransactionUnspentOutput)
import Serialization.WitnessSet as SW
import Test.Fixtures
  ( nativeScriptFixture1
  , nativeScriptFixture2
  , nativeScriptFixture3
  , nativeScriptFixture4
  , nativeScriptFixture5
  , nativeScriptFixture6
  , nativeScriptFixture7
  , plutusDataFixture1
  , plutusDataFixture2
  , plutusDataFixture3
  , plutusDataFixture4
  , plutusDataFixture5
  , plutusDataFixture6
  , plutusDataFixture7
  , txInputFixture1
  , txOutputFixture1
  , utxoFixture1
  , utxoFixture1'
  , witnessSetFixture1
  , witnessSetFixture2
  , witnessSetFixture2Value
  , witnessSetFixture3
  , witnessSetFixture3Value
  , witnessSetFixture4
  )
import Test.Utils (errMaybe)
import Types.Transaction (NativeScript(ScriptAny), TransactionInput, TransactionOutput) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput)) as T

suite :: TestPlanM Unit
suite = do
  group "deserialization" $ do
    group "BigInt" do
      test "Deserialization is inverse to serialization" do
        let bigInt = BigInt.fromInt 123
        res <- errMaybe "Failed to serialize BigInt" do
          DB.convertBigInt =<< SB.convertBigInt bigInt
        res `shouldEqual` bigInt
    group "BigNum" do
      test "Deserialization is inverse to serialization" do
        let bigInt = BigInt.fromInt 123
        res <- errMaybe "Failed to serialize BigInt" $ bigNumFromBigInt bigInt >>= bigNumToBigInt
        res `shouldEqual` bigInt
    group "PlutusData: deserialization is inverse to serialization" do
      test "fixture #1" do
        let input = plutusDataFixture1
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #2" do
        let input = plutusDataFixture2
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #3" do
        let input = plutusDataFixture3
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #4" do
        let input = plutusDataFixture4
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #5" do
        let input = plutusDataFixture5
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #6" do
        let input = plutusDataFixture6
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #7" do
        let input = plutusDataFixture7
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
      test "fixture #1" do
        let input = plutusDataFixture1
        res <- errMaybe "Failed to convert PlutusData" $
          SPD.convertPlutusData input >>= DPD.convertPlutusData
        res `shouldEqual` input
    group "UnspentTransactionOutput" do
      test "deserialization is inverse to serialization" do
        unspentOutput <- liftEffect $ createUnspentOutput txInputFixture1 txOutputFixture1
        T.TransactionUnspentOutput { input, output } <-
          errMaybe "Failed deserialization 3" do
            convertUnspentOutput unspentOutput
        input `shouldEqual` txInputFixture1
        output `shouldEqual` txOutputFixture1
      test "fixture #1" do
        res <- errMaybe "Failed deserialization 4" do
          newTransactionUnspentOutputFromBytes utxoFixture1 >>= convertUnspentOutput
        res `shouldEqual` utxoFixture1'
    group "WitnessSet - deserialization" do
      group "fixture #1" do
        res <- errMaybe "Failed deserialization 5" do
          deserializeWitnessSet witnessSetFixture1 >>= convertWitnessSet
        test "has vkeys" do
          (unwrap res).vkeys `shouldSatisfy` isJust
        test "has plutusData" do
          (unwrap res).plutusData `shouldSatisfy` isJust
        test "has plutusScripts" do
          (unwrap res).plutusScripts `shouldSatisfy` isJust
        test "has redeemers" do
          (unwrap res).redeemers `shouldSatisfy` isJust
        test "has redeemers" do
          (unwrap res).redeemers `shouldSatisfy` isJust
        test "does not have nativeScripts" do
          (unwrap res).nativeScripts `shouldSatisfy` isNothing
      test "fixture #2" do
        res <- errMaybe "Failed deserialization 6" do
          deserializeWitnessSet witnessSetFixture2 >>= convertWitnessSet
        res `shouldEqual` witnessSetFixture2Value
      test "fixture #3" do
        res <- errMaybe "Failed deserialization 7" do
          deserializeWitnessSet witnessSetFixture3 >>= convertWitnessSet
        res `shouldEqual` witnessSetFixture3Value
      group "fixture #4" do
        res <- errMaybe "Failed deserialization 8" $
          deserializeWitnessSet witnessSetFixture4 >>= convertWitnessSet
        test "has nativeScripts" do
          (unwrap res).nativeScripts `shouldSatisfy` isJust
    group "NativeScript - deserializaton is inverse to serialization" do
      test "fixture #1" do
        liftEffect $ testNativeScript nativeScriptFixture1
      test "fixture #2" do
        liftEffect $ testNativeScript nativeScriptFixture2
      test "fixture #3" do
        liftEffect $ testNativeScript nativeScriptFixture3
      test "fixture #4" do
        liftEffect $ testNativeScript nativeScriptFixture4
      test "fixture #5" do
        liftEffect $ testNativeScript nativeScriptFixture5
      test "fixture #6" do
        liftEffect $ testNativeScript nativeScriptFixture6
      test "fixture #7" do
        liftEffect $ testNativeScript nativeScriptFixture7
      test "fixture #7" do
        liftEffect $ testNativeScript nativeScriptFixture7
      -- This is here just to acknowledge the problem
      test "too much nesting leads to recursion error" do
        expectError $ do
          let
            longNativeScript =
              Array.foldr (\_ acc -> T.ScriptAny [ acc ]) nativeScriptFixture1 $
                Array.range 0 5000
          liftEffect $ testNativeScript longNativeScript
    group "WitnessSet - deserialization is inverse to serialization" do
      test "fixture #1" do
        ws0 <- errMaybe "Failed deserialization" $
          deserializeWitnessSet witnessSetFixture1 >>= convertWitnessSet
        ws1 <- liftEffect $ SW.convertWitnessSet ws0
        ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
        ws0 `shouldEqual` ws2 -- value representation
        let wsBytes = Serialization.toBytes (asOneOf ws1)
        wsBytes `shouldEqual` witnessSetFixture1 -- byte representation
      test "fixture #2" do
        ws0 <- errMaybe "Failed deserialization" $
          deserializeWitnessSet witnessSetFixture2 >>= convertWitnessSet
        ws1 <- liftEffect $ SW.convertWitnessSet ws0
        ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
        ws0 `shouldEqual` ws2 -- value representation
        let wsBytes = Serialization.toBytes (asOneOf ws1)
        wsBytes `shouldEqual` witnessSetFixture2 -- byte representation
      test "fixture #3" do
        ws0 <- errMaybe "Failed deserialization" $
          deserializeWitnessSet witnessSetFixture3 >>= convertWitnessSet
        ws1 <- liftEffect $ SW.convertWitnessSet ws0
        ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
        ws0 `shouldEqual` ws2 -- value representation
        let wsBytes = Serialization.toBytes (asOneOf ws1)
        wsBytes `shouldEqual` witnessSetFixture3 -- byte representation
      -- TODO: enable when nativeScripts are implemented
      test "fixture #4" do
        ws0 <- errMaybe "Failed deserialization" $
          deserializeWitnessSet witnessSetFixture4 >>= convertWitnessSet
        ws1 <- liftEffect $ SW.convertWitnessSet ws0
        ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
        ws0 `shouldEqual` ws2 -- value representation
        let wsBytes = Serialization.toBytes (asOneOf ws1)
        wsBytes `shouldEqual` witnessSetFixture4 -- byte representation

createUnspentOutput :: T.TransactionInput -> T.TransactionOutput -> Effect TransactionUnspentOutput
createUnspentOutput input output = do
  input' <- Serialization.convertTxInput input
  output' <- Serialization.convertTxOutput output
  pure $ mkTransactionUnspentOutput input' output'

testNativeScript :: T.NativeScript -> Effect Unit
testNativeScript input = do
  serialized <- errMaybe "Failed serialization" $ NSS.convertNativeScript input
  let bytes = Serialization.toBytes (asOneOf serialized)
  res <- errMaybe "Failed deserialization" $ fromBytes bytes
  res' <- errMaybe "Failed deserialization" $ NSD.convertNativeScript res
  res' `shouldEqual` input
