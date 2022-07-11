module Test.Serialization (suite) where

import Prelude

import Data.Maybe (isJust)
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Deserialization.FromBytes (fromBytesEffect)
import Effect.Class (liftEffect)
import Mote (group, test)
import Serialization (convertTransaction, convertTxOutput, toBytes)
import Serialization.PlutusData (convertPlutusData)
import Serialization.Types (TransactionHash)
import Test.Fixtures
  ( txBinaryFixture1
  , txBinaryFixture2
  , txBinaryFixture3
  , txBinaryFixture4
  , txBinaryFixture5
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
  , txFixture5
  , txOutputBinaryFixture1
  , txOutputFixture1
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (errMaybe)
import TestM (TestPlanM)
import Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Types.PlutusData as PD
import Untagged.Union (asOneOf)

suite :: TestPlanM Unit
suite = do
  group "cardano-serialization-lib bindings" $ do
    group "conversion between types" $ do
      test "newTransactionHash" do
        let
          txString =
            "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996"
          txBytes = hexToByteArrayUnsafe txString
        _txHash :: TransactionHash <- liftEffect $ fromBytesEffect txBytes
        pure unit
      test "PlutusData #1 - Constr" $ do
        let
          datum = PD.Constr (BigInt.fromInt 1)
            [ PD.Integer (BigInt.fromInt 1)
            , PD.Integer (BigInt.fromInt 2)
            ]
        (convertPlutusData datum $> unit) `shouldSatisfy` isJust
      test "PlutusData #2 - Map" $ do
        let
          datum =
            PD.Map
              [ PD.Integer (BigInt.fromInt 1) /\ PD.Integer (BigInt.fromInt 2)
              , PD.Integer (BigInt.fromInt 3) /\ PD.Integer (BigInt.fromInt 4)
              ]
        (convertPlutusData datum $> unit) `shouldSatisfy` isJust
      test "PlutusData #3 - List" $ do
        let
          datum = PD.List
            [ PD.Integer (BigInt.fromInt 1), PD.Integer (BigInt.fromInt 2) ]
        (convertPlutusData datum $> unit) `shouldSatisfy` isJust
      test "PlutusData #4 - List" $ do
        let
          datum = PD.List
            [ PD.Integer (BigInt.fromInt 1), PD.Integer (BigInt.fromInt 2) ]
        (convertPlutusData datum $> unit) `shouldSatisfy` isJust
      test "PlutusData #5 - Bytes" $ do
        let datum = PD.Bytes $ hexToByteArrayUnsafe "00ff"
        (convertPlutusData datum $> unit) `shouldSatisfy` isJust
      test
        "PlutusData #6 - Integer 0 (regression to https://github.com/Plutonomicon/cardano-transaction-lib/issues/488 ?)"
        $ do
            let
              datum = PD.Integer (BigInt.fromInt 0)
            datum' <- errMaybe "Cannot convertPlutusData" $ convertPlutusData
              datum
            let bytes = toBytes (asOneOf datum')
            byteArrayToHex bytes `shouldEqual` "00"
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
      test "Transaction serialization #4 - ada + mint + certificates" $
        liftEffect do
          tx <- convertTransaction txFixture4
          let bytes = toBytes (asOneOf tx)
          byteArrayToHex bytes `shouldEqual` txBinaryFixture4
      test "Transaction serialization #5 - plutusv2" $ liftEffect do
        tx <- convertTransaction txFixture5
        let bytes = toBytes (asOneOf tx)
        byteArrayToHex bytes `shouldEqual` txBinaryFixture5
