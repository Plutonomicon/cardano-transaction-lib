module Test.Deserialization (suite) where

import Prelude

import Data.BigInt as BigInt
import Data.Maybe (isJust, isNothing)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Mote (group, test, skip)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Untagged.Union (asOneOf)

import Deserialization.Address (convertAddress)
import Deserialization.BigNum (bigNumToBigInt)
import Deserialization.UnspentOutput (convertUnspentOutput, mkTransactionUnspentOutput, newTransactionUnspentOutputFromBytes)
import Deserialization.WitnessSet (deserializeWitnessSet, convertWitnessSet)
import Serialization as Serialization
import Serialization.BigNum (bigNumFromBigInt)
import Serialization.Types (TransactionUnspentOutput)
import Serialization.WitnessSet as SW
import Test.Fixtures (addressString1, txInputFixture1, txOutputFixture1, utxoFixture1, utxoFixture1', witnessSetFixture1, witnessSetFixture2, witnessSetFixture2Value, witnessSetFixture3, witnessSetFixture3Value, witnessSetFixture4)
import Test.Utils (errMaybe)
import Types.Transaction (Bech32(Bech32), TransactionInput, TransactionOutput) as T
import Types.TransactionUnspentOutput (TransactionUnspentOutput(TransactionUnspentOutput)) as T

suite :: TestPlanM Unit
suite = do
  group "deserialization" $ do
    group "BigNum" do
      test "Deserialization is inverse to serialization" do
        let bigInt = BigInt.fromInt 123
        res <- errMaybe "Failed to serialize BigInt" $ bigNumFromBigInt bigInt >>= bigNumToBigInt
        res `shouldEqual` bigInt
    group "Address" do
      test "deserialization works" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString1)
        convertAddress address `shouldSatisfy` isJust
      test "deserialization is inverse to serialization" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString1)
        address' <- errMaybe "Failed deserialization 1" do
          convertAddress address
        address'' <- liftEffect $ Serialization.convertAddress address'
        address''' <- errMaybe "Failed deserialization 2" do
          convertAddress address''
        address''' `shouldEqual` address'
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
        test "has plutus_data" do
          (unwrap res).plutus_data `shouldSatisfy` isJust
        test "has plutus_scripts" do
          (unwrap res).plutus_scripts `shouldSatisfy` isJust
        test "has redeemers" do
          (unwrap res).redeemers `shouldSatisfy` isJust
        test "has redeemers" do
          (unwrap res).redeemers `shouldSatisfy` isJust
        test "does not have native_scripts" do
          (unwrap res).native_scripts `shouldSatisfy` isNothing
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
        test "has native_scripts" do
          (unwrap res).native_scripts `shouldSatisfy` isJust
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
      -- TODO: enable when native_scripts are implemented
      skip $ test "fixture #4" do
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
