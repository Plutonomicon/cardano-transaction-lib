module Test.Serialization (suite) where

import Prelude

import Deserialization.FromBytes (fromBytesEffect)
import Effect.Class (liftEffect)
import Mote (group, test)
import Serialization (convertTransaction, convertTxOutput, toBytes)
import Serialization.Types (TransactionHash)
import Test.Fixtures (txBinaryFixture1, txBinaryFixture2, txBinaryFixture3, txFixture1, txFixture2, txFixture3, txOutputBinaryFixture1, txOutputFixture1)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Untagged.Union (asOneOf)

suite :: TestPlanM Unit
suite = do
  group "cardano-serialization-lib bindings" $ do
    group "conversion between types" $ do
      test "newTransactionHash" do
        let
          txString = "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          txBytes = hexToByteArrayUnsafe txString
        _txHash :: TransactionHash <- liftEffect $ fromBytesEffect txBytes
        pure unit
      test "TransactionOutput serialization" $ liftEffect do
        txo <- convertTxOutput txOutputFixture1
        let bytes = toBytes (asOneOf txo)
        byteArrayToHex bytes `shouldEqual` txOutputBinaryFixture1
      test "Transaction serialization #1" $ liftEffect do
        tx <- convertTransaction txFixture1
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture1
      test "Transaction serialization #2 - tokens" $ liftEffect do
        tx <- convertTransaction txFixture2
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture2
      test "Transaction serialization #3 - ada" $ liftEffect do
        tx <- convertTransaction txFixture3
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture3
