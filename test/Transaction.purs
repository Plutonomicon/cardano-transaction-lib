module Test.Ctl.Transaction (suite) where

import Prelude

import Ctl.Internal.Cardano.Types.Transaction
  ( Redeemer(Redeemer)
  , ScriptDataHash(ScriptDataHash)
  , Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , TxBody(TxBody)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , mkEd25519Signature
  , mkPublicKey
  )
import Ctl.Internal.Deserialization.WitnessSet as Deserialization.WitnessSet
import Ctl.Internal.Serialization.WitnessSet as Serialization.WitnessSet
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Transaction
  ( attachDatum
  , attachPlutusScript
  , attachRedeemer
  , setScriptDataHash
  )
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Ctl.Internal.Types.Datum (Datum(Datum))
import Ctl.Internal.Types.PlutusData (PlutusData(Integer))
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Spend))
import Ctl.Internal.Types.Scripts
  ( Language(PlutusV1, PlutusV2)
  , PlutusScript(PlutusScript)
  )
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import JS.BigInt as BigInt
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures.CostModels (costModelsFixture1)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = group "attach datums to tx" $ do
  test "datum should be correctly attached" testAttachDatum
  test "redeemer should be correctly attached" testAttachRedeemer
  test "scripts should be correctly attached (PlutusV1)" $ testAttachScript
    PlutusV1
  test "scripts should be correctly attached (PlutusV2)" $ testAttachScript
    PlutusV2
  test "scripts data hash should be correctly set" testSetScriptDataHash
  test "existing witnesses should be preserved" testPreserveWitness

testAttachDatum :: Aff Unit
testAttachDatum = liftEffect $
  attachDatum datum tx >>= case _ of
    Transaction { witnessSet: TransactionWitnessSet ws } ->
      case ws.plutusData of
        Just [ pd ] -> do
          pd `shouldEqual` unwrap datum
        Just _ -> throw "Incorrect number of datums attached"
        Nothing -> throw "Datum wasn't attached"
  where
  tx :: Transaction
  tx = mempty

  datum :: Datum
  datum = Datum $ Integer $ BigInt.fromInt 1

testAttachRedeemer :: Aff Unit
testAttachRedeemer = liftEffect $ do
  redeemer <- mkRedeemer datum
  attachRedeemer redeemer tx >>= case _ of
    Transaction { witnessSet: TransactionWitnessSet ws } -> do
      case ws.redeemers of
        Just [ r ] -> r `shouldEqual` redeemer
        Just _ -> throw "Incorrect number of redeemers attached"
        Nothing -> throw "Redeemer wasn't attached"
  where
  tx :: Transaction
  tx = mempty

  datum :: PlutusData
  datum = Integer $ BigInt.fromInt 1

testAttachScript :: Language -> Aff Unit
testAttachScript language = liftEffect $
  attachPlutusScript script tx >>= case _ of
    Transaction { witnessSet: TransactionWitnessSet ws } ->
      case ws.plutusScripts of
        Just [ ps ] -> ps `shouldEqual` script
        Just _ -> throw "Incorrect number of scripts attached"
        Nothing -> throw "Script wasn't attached"
  where
  tx :: Transaction
  tx = mempty

  script :: PlutusScript
  script = PlutusScript $ hexToByteArrayUnsafe "4e4d01000033222220051200120011"
    /\ language

testSetScriptDataHash :: Aff Unit
testSetScriptDataHash = liftEffect $ do
  redeemer <- mkRedeemer datum2
  Transaction { body: TxBody body } <-
    setScriptDataHash costModelsFixture1 [ redeemer ] [ datum1 ] tx
  case body.scriptDataHash of
    Nothing -> throw "Script data hash wasn't set"
    Just (ScriptDataHash sdh) ->
      -- TODO
      -- Verify the hash with some external tool
      byteArrayToHex sdh
        `shouldEqual`
          "dde4d4f89151fdbcea51fa4e9edcac89d69febe036a4976b643c713c19b0cad1"
  where
  tx :: Transaction
  tx = mempty

  datum1 :: Datum
  datum1 = Datum $ Integer $ BigInt.fromInt 1

  datum2 :: PlutusData
  datum2 = Integer $ BigInt.fromInt 2

testPreserveWitness :: Aff Unit
testPreserveWitness = liftEffect $ do
  Transaction { witnessSet: TransactionWitnessSet { plutusData, vkeys } } <-
    attachDatum datum tx
  case plutusData /\ vkeys of
    Just [ pd ] /\ Just vs@[ _ ] -> do
      pd `shouldEqual` unwrap datum
      vk' <- Deserialization.WitnessSet.convertVkeyWitnesses <$>
        Serialization.WitnessSet.convertVkeywitnesses vs

      vk' `shouldEqual` [ vk ]
    Just _ /\ Just _ -> throw "Incorrect number of witnesses"
    Nothing /\ _ -> throw "Datum wasn't attached"
    _ /\ Nothing -> throw "Vkey witness wasn't preserved"
  where
  tx :: Transaction
  tx = over Transaction _ { witnessSet = initialWitnessSet }
    $ mempty

  datum :: Datum
  datum = Datum $ Integer $ BigInt.fromInt 1

  initialWitnessSet :: TransactionWitnessSet
  initialWitnessSet = over TransactionWitnessSet _ { vkeys = Just [ vk ] }
    $ mempty

  vk :: Vkeywitness
  vk = Vkeywitness
    ( Vkey
        ( unsafePartial $ fromJust <<< mkPublicKey $
            "ed25519_pk1p9sf9wz3t46u9ghht44203gerxt82kzqaqw74fqrmwjmdy8sjxmqknzq8j"
        )
        /\
          ( unsafePartial $ fromJust <<< mkEd25519Signature $
              "ed25519_sig1clmhgxx9e9t24wzgkmcsr44uq98j935evsjnrj8nn7ge08qrz0mgdx\
              \v5qtz8dyghs47q3lxwk4akq3u2ty8v4egeqvtl02ll0nfcqqq6faxl6"
          )
    )

mkRedeemer :: PlutusData -> Effect Redeemer
mkRedeemer pd = do
  pure $ Redeemer
    { tag: Spend
    , index: BigInt.fromInt 0
    , data: pd
    , exUnits:
        { mem: BigInt.fromInt 7000000
        , steps: BigInt.fromInt 300000000
        }
    }
