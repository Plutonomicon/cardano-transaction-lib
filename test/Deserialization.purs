module Test.Ctl.Deserialization (suite) where

import Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType(Other)
  , decodeTextEnvelope
  )
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript(ScriptAny)) as T
import Ctl.Internal.Cardano.Types.Transaction (Transaction, TransactionOutput) as T
import Ctl.Internal.Cardano.Types.Transaction (Vkeywitness)
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as T
import Ctl.Internal.Deserialization.BigInt as DB
import Ctl.Internal.Deserialization.FromBytes (fromBytes, fromBytesEffect)
import Ctl.Internal.Deserialization.NativeScript as NSD
import Ctl.Internal.Deserialization.PlutusData as DPD
import Ctl.Internal.Deserialization.Transaction (convertTransaction) as TD
import Ctl.Internal.Deserialization.UnspentOutput
  ( convertUnspentOutput
  , mkTransactionUnspentOutput
  )
import Ctl.Internal.Deserialization.WitnessSet (convertWitnessSet)
import Ctl.Internal.Serialization (convertTransaction) as TS
import Ctl.Internal.Serialization (convertTxInput, convertTxOutput) as Serialization
import Ctl.Internal.Serialization.BigInt as SB
import Ctl.Internal.Serialization.NativeScript (convertNativeScript) as NSS
import Ctl.Internal.Serialization.PlutusData as SPD
import Ctl.Internal.Serialization.ToBytes (toBytes)
import Ctl.Internal.Serialization.ToBytes (toBytes) as Serialization
import Ctl.Internal.Serialization.Types (TransactionUnspentOutput)
import Ctl.Internal.Serialization.Types (Vkeywitness) as Serialization
import Ctl.Internal.Serialization.WitnessSet (convertVkeywitness) as Serialization
import Ctl.Internal.Serialization.WitnessSet as SW
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.BigNum (fromBigInt, toBigInt) as BigNum
import Ctl.Internal.Types.Transaction (TransactionInput) as T
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (fold)
import Data.Maybe (isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import JS.BigInt as BigInt
import Mote (group, skip, test)
import Test.Ctl.Fixtures
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
  , txFixture5
  , txFixture6
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
import Test.Ctl.Utils (errMaybe)
import Test.Spec.Assertions (expectError, shouldEqual, shouldSatisfy)

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
          <#> BigNum.toBigInt
        res `shouldEqual` bigInt
    group "CSL <-> CTL PlutusData roundtrip tests" do
      let
        pdRoundTripTest ctlPd = do
          cslPd' <- errMaybe "Failed to fromBytes PlutusData" $ fromBytes
            $ Serialization.toBytes
            $ SPD.convertPlutusData ctlPd
          let ctlPd' = DPD.convertPlutusData cslPd'
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
              $ wrap plutusDataFixture8Bytes
            let ctlPd' = DPD.convertPlutusData cslPd'
            ctlPd' `shouldEqual` plutusDataFixture8
            cslPdWp' <- errMaybe "Failed to fromBytes PlutusData" $ fromBytes
              $ wrap plutusDataFixture8Bytes'
            let ctlPdWp' = DPD.convertPlutusData cslPdWp'
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
          fromBytes (wrap utxoFixture1) >>=
            convertUnspentOutput
        res `shouldEqual` utxoFixture1'
    group "Transaction Roundtrips" do
      test "CSL <-> CTL Transaction roundtrip #1" $ txRoundtrip txFixture1
      test "CSL <-> CTL Transaction roundtrip #2" $ txRoundtrip txFixture2
      test "CSL <-> CTL Transaction roundtrip #3" $ txRoundtrip txFixture3
      test "CSL <-> CTL Transaction roundtrip #4" $ txRoundtrip txFixture4
      test "CSL <-> CTL Transaction roundtrip #5" $ txRoundtrip txFixture5
      test "CSL <-> CTL Transaction roundtrip #6" $ txRoundtrip txFixture6
    group "WitnessSet - deserialization" do
      group "fixture #1" do
        res <- errMaybe "Failed deserialization 5" do
          fromBytes (wrap witnessSetFixture1) >>= convertWitnessSet
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
          fromBytes (wrap witnessSetFixture2) >>= convertWitnessSet
        res `shouldEqual` witnessSetFixture2Value
      test "fixture #3" do
        res <- errMaybe "Failed deserialization 7" do
          fromBytes (wrap witnessSetFixture3) >>= convertWitnessSet
        res `shouldEqual` witnessSetFixture3Value
      group "fixture #4" do
        res <- errMaybe "Failed deserialization 8" $
          fromBytes (wrap witnessSetFixture4) >>= convertWitnessSet
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
      skip $ test "too much nesting leads to recursion error" do
        expectError $ do
          let
            longNativeScript =
              Array.foldr (\_ acc -> T.ScriptAny [ acc ]) nativeScriptFixture1 $
                Array.range 0 50 -- change this to 50000
          liftEffect $ testNativeScript longNativeScript
    group "WitnessSet - deserialization is inverse to serialization" do
      let
        vkeyWitnessesRoundtrip
          :: ∀ (m :: Type -> Type)
           . MonadEffect m
          => MonadThrow Error m
          => Array Vkeywitness
          -> m Unit
        vkeyWitnessesRoundtrip vks = do
          cslVks <- traverse (liftEffect <<< Serialization.convertVkeywitness)
            vks
          let cslVksBytes = toBytes <$> cslVks
          (_ :: Array Serialization.Vkeywitness) <- traverse
            (liftEffect <<< fromBytesEffect)
            cslVksBytes
          pure unit

        witnessSetRoundTrip
          :: ∀ (m :: Type -> Type)
           . MonadEffect m
          => MonadThrow Error m
          => ByteArray
          -> m Unit
        witnessSetRoundTrip fixture = do
          ws0 <- errMaybe "Failed deserialization" $
            fromBytes (wrap fixture) >>= convertWitnessSet
          ws1 <- liftEffect $ SW.convertWitnessSet ws0
          ws2 <- errMaybe "Failed deserialization" $ convertWitnessSet ws1
          let vkeys = fold (unwrap ws2).vkeys
          vkeyWitnessesRoundtrip vkeys
          ws0 `shouldEqual` ws2 -- value representation
          let wsBytes = unwrap $ Serialization.toBytes ws1
          wsBytes `shouldEqual` fixture -- byte representation
      test "fixture #1" $ witnessSetRoundTrip witnessSetFixture1
      test "fixture #2" $ witnessSetRoundTrip witnessSetFixture2
      test "fixture #3" $ witnessSetRoundTrip witnessSetFixture3
      -- TODO: enable when nativeScripts are implemented
      test "fixture #4" $ witnessSetRoundTrip witnessSetFixture4
    group "TextEnvelope decoding" do
      test "Decoding TestEnvelope with some other type" do
        let
          otherTypeTextEnvelope =
            """
              {
                "cborHex": "484701000022120011",
                "description": "other-type-text-envelope",
                "type": "SomeOtherType"
              }
              """
        TextEnvelope envelope <- liftMaybe (error "Unexpected parsing error") $
          decodeTextEnvelope otherTypeTextEnvelope
        envelope.type_ `shouldEqual` (Other "SomeOtherType")

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
  serialized <- pure $ NSS.convertNativeScript input
  {-            ^^^^ This is necessary here as convertNativeScript can throw
                a maximum call stack size runtime error (see excessive nesting
                test above). It needs to be lifted into the Effect monad for
                purescript to handle it correctly.
  -}

  let bytes = Serialization.toBytes serialized
  res <- errMaybe "Failed deserialization" $ fromBytes bytes
  let
    res' = NSD.convertNativeScript res
  res' `shouldEqual` input

txRoundtrip :: T.Transaction -> Aff Unit
txRoundtrip tx = do
  cslTX <- liftEffect $ TS.convertTransaction tx
  expected <- errMaybe "Cannot convert TX from CSL to CTL" $ hush $
    TD.convertTransaction cslTX
  tx `shouldEqual` expected
