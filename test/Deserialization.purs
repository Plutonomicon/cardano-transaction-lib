module Test.Deserialization (suite) where

import Prelude

import Cardano.Types.Transaction (NativeScript(ScriptAny), TransactionOutput) as T
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as T
import Contract.Address (ByteArray)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (hush)
import Data.Maybe (isJust, isNothing)
import Data.Newtype (unwrap)
import Deserialization.BigInt as DB
import Deserialization.FromBytes (fromBytes)
import Deserialization.NativeScript as NSD
import Deserialization.PlutusData as DPD
import Deserialization.Transaction (convertTransaction) as TD
import Deserialization.UnspentOutput
  ( convertUnspentOutput
  , mkTransactionUnspentOutput
  , newTransactionUnspentOutputFromBytes
  )
import Serialization (convertTransaction) as TS
import Deserialization.WitnessSet (convertWitnessSet, deserializeWitnessSet)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Mote (group, test)
import Serialization (toBytes)
import Serialization as Serialization
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
  , plutusDataFixture8
  , plutusDataFixture8Bytes
  , plutusDataFixture8Bytes'
  , txFixture1
  , txFixture2
  , txFixture3
  , txFixture4
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
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, expectError)
import Test.Utils (errMaybe)
import TestM (TestPlanM)
import Types.BigNum (fromBigInt, toBigInt) as BigNum
import Types.Transaction (TransactionInput) as T
import Untagged.Union (asOneOf)

suite :: TestPlanM (Aff Unit) Unit
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
        res <- errMaybe "Failed to serialize BigInt" $ BigNum.fromBigInt bigInt
          >>= BigNum.toBigInt
        res `shouldEqual` bigInt
    group "CSL <-> CTL PlutusData roundtrip tests" do
      let
        pdRoundTripTest ctlPd = do
          cslPd <-
            errMaybe "Failed to convert from CTL PlutusData to CSL PlutusData" $
              SPD.convertPlutusData ctlPd
          let pdBytes = toBytes (asOneOf cslPd)
          cslPd' <- errMaybe "Failed to fromBytes PlutusData" $ fromBytes
            pdBytes
          ctlPd' <-
            errMaybe "Failed to convert from CSL PlutusData to CTL PlutusData" $
              DPD.convertPlutusData cslPd'
          ctlPd' `shouldEqual` ctlPd
      test "fixture #1" $ pdRoundTripTest plutusDataFixture1
      test "fixture #2" $ pdRoundTripTest plutusDataFixture2
      test "fixture #3" $ pdRoundTripTest plutusDataFixture3
      test "fixture #4" $ pdRoundTripTest plutusDataFixture4
      test "fixture #5" $ pdRoundTripTest plutusDataFixture5
      test "fixture #6" $ pdRoundTripTest plutusDataFixture6
      test "fixture #7" $ pdRoundTripTest plutusDataFixture7
      test "fixture #8" $ pdRoundTripTest plutusDataFixture8
      test
        "fixture #8 different Cbor bytes encodings (compact vs general Constr tag encodings)"
        $ do
            cslPd' <- errMaybe "Failed to fromBytes PlutusData" $ fromBytes
              plutusDataFixture8Bytes
            ctlPd' <-
              errMaybe "Failed to convert from CSL PlutusData to CTL PlutusData"
                $
                  DPD.convertPlutusData cslPd'
            ctlPd' `shouldEqual` plutusDataFixture8
            cslPdWp' <- errMaybe "Failed to fromBytes PlutusData" $ fromBytes
              plutusDataFixture8Bytes'
            ctlPdWp' <-
              errMaybe "Failed to convert from CSL PlutusData to CTL PlutusData"
                $
                  DPD.convertPlutusData cslPdWp'
            ctlPdWp' `shouldEqual` plutusDataFixture8
    group "UnspentTransactionOutput" do
      test "deserialization is inverse to serialization" do
        unspentOutput <- liftEffect $ createUnspentOutput txInputFixture1
          txOutputFixture1
        T.TransactionUnspentOutput { input, output } <-
          errMaybe "Failed deserialization 3" do
            convertUnspentOutput unspentOutput
        input `shouldEqual` txInputFixture1
        output `shouldEqual` txOutputFixture1
      test "fixture #1" do
        res <- errMaybe "Failed deserialization 4" do
          newTransactionUnspentOutputFromBytes utxoFixture1 >>=
            convertUnspentOutput
        res `shouldEqual` utxoFixture1'
    group "Transaction" do
      test "deserialization is inverse to serialization #1" do
        let input = txFixture1
        serialized <- liftEffect $ TS.convertTransaction input
        let expected = TD.convertTransaction serialized
        pure input `shouldEqual` hush expected
      test "deserialization is inverse to serialization #2" do
        let input = txFixture2
        serialized <- liftEffect $ TS.convertTransaction input
        let expected = TD.convertTransaction serialized
        pure input `shouldEqual` hush expected
      test "deserialization is inverse to serialization #3" do
        let input = txFixture3
        serialized <- liftEffect $ TS.convertTransaction input
        let expected = TD.convertTransaction serialized
        pure input `shouldEqual` hush expected
      test "deserialization is inverse to serialization #4" do
        let input = txFixture4
        serialized <- liftEffect $ TS.convertTransaction input
        let expected = TD.convertTransaction serialized
        pure input `shouldEqual` hush expected
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
      let
        witnessSetRoundTrip
          :: ∀ (m :: Type -> Type)
           . MonadEffect m
          => MonadThrow Error m
          => ByteArray
          -> m Unit
        witnessSetRoundTrip fixture = do
          ws0 <- errMaybe "Failed deserialization" $
            deserializeWitnessSet fixture >>= convertWitnessSet
          ws1 <- liftEffect $ SW.convertWitnessSet ws0
          ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
          ws0 `shouldEqual` ws2 -- value representation
          let wsBytes = Serialization.toBytes (asOneOf ws1)
          wsBytes `shouldEqual` fixture -- byte representation
      test "fixture #1" $ witnessSetRoundTrip witnessSetFixture1
      test "fixture #2" $ witnessSetRoundTrip witnessSetFixture2
      test "fixture #3" $ witnessSetRoundTrip witnessSetFixture3
      -- TODO: enable when nativeScripts are implemented
      test "fixture #4" $ witnessSetRoundTrip witnessSetFixture4

createUnspentOutput
  :: T.TransactionInput
  -> T.TransactionOutput
  -> Effect TransactionUnspentOutput
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
