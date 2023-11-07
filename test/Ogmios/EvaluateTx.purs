module Test.Ctl.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch))
import Ctl.Internal.QueryM.JsonRpc2
  ( OgmiosDecodeError(DecodingError)
  , decodeOgmiosResponse
  )
import Ctl.Internal.QueryM.Ogmios
  ( ExecutionUnits
  , RedeemerPointer
  , TxEvaluationR(TxEvaluationR)
  , TxEvaluationResult(TxEvaluationResult)
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.Natural (fromInt')
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Data.Either (Either(Left, Right))
import Data.Map (fromFoldable) as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
-- import Test.Ctl.Fixtures (ogmiosEvaluateTxFailIncompatibleEraFixture, ogmiosEvaluateTxFailScriptErrorsFixture, ogmiosEvaluateTxInvalidPointerFormatFixture, ogmiosEvaluateTxValidRespFixture)
import Test.Ctl.Fixtures
  ( ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          decodeOgmiosResponse <$> liftEffect ogmiosEvaluateTxValidRespFixture
        txEvalR `shouldSatisfy` case _ of
          Right (TxEvaluationR (Right (TxEvaluationResult map))) -> map ==
            Map.fromFoldable ogmiosEvaluateTxValidRespDecoded
          _ -> false

      test "Fails to decode a response with invalid redeemer pointer format" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          decodeOgmiosResponse <$> liftEffect
            ogmiosEvaluateTxInvalidPointerFormatFixture
        txEvalR `shouldSatisfy` case _ of
          Left (DecodingError (TypeMismatch _)) -> true
          _ -> false

-- test "Successfully decodes a failed execution response (Incompatible era)" do
--   txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
--     decodeOgmiosResponse <$> liftEffect ogmiosEvaluateTxFailIncompatibleEraFixture
--   (Map.toUnfoldable <<< unwrap <$> txEvalR) `shouldEqual`
--     Right (TxEvaluationResult ogmiosEvaluateTxValidRespDecoded)

-- test "Successfully decodes a failed execution response (Script errors)" do
--   txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
--     decodeOgmiosResponse <$> liftEffect ogmiosEvaluateTxFailScriptErrorsFixture
--   (Map.toUnfoldable <<< unwrap <$> txEvalR) `shouldEqual`
--     Right (TxEvaluationResult ogmiosEvaluateTxValidRespDecoded)

ogmiosEvaluateTxValidRespDecoded :: Array (RedeemerPointer /\ ExecutionUnits)
ogmiosEvaluateTxValidRespDecoded =
  [ { redeemerTag: Mint, redeemerIndex: zero }
      /\ { memory: fromInt' 1685698, steps: fromInt' 609724445 }
  , { redeemerTag: Spend, redeemerIndex: one }
      /\ { memory: fromInt' 1700, steps: fromInt' 476468 }
  ]
