module Test.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (decodeAeson, JsonDecodeError(TypeMismatch))
import Data.Either (Either(Left, Right))
import Data.Map (toUnfoldable) as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.Fixtures
  ( ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import TestM (TestPlanM)
import Types.Natural (fromInt')
import Types.RedeemerTag (RedeemerTag(Mint, Spend))
import QueryM.Ogmios
  ( TxEvaluationR(TxEvaluationFailure, TxEvaluationSuccess)
  , ExecutionUnits
  , RedeemerPointer
  )

suite :: TestPlanM Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either JsonDecodeError TxEvaluationR <-
          decodeAeson <$> liftEffect ogmiosEvaluateTxValidRespFixture
        case txEvalR of
          Left err -> fail $ "JSON decodeing failed with:" <> show err
          Right (TxEvaluationFailure aeson) -> fail $ "Evaluation failed with:"
            <> show aeson
          Right (TxEvaluationSuccess response) -> Map.toUnfoldable response
            `shouldEqual` ogmiosEvaluateTxValidRespDecoded

      test "Fails to decode a response with invalid redeemer pointer format" do
        txEvalR :: Either JsonDecodeError TxEvaluationR <-
          decodeAeson <$> liftEffect ogmiosEvaluateTxInvalidPointerFormatFixture
        txEvalR `shouldSatisfy` case _ of
          Left (TypeMismatch _) -> true
          _ -> false

ogmiosEvaluateTxValidRespDecoded :: Array (RedeemerPointer /\ ExecutionUnits)
ogmiosEvaluateTxValidRespDecoded =
  [ { redeemerTag: Mint, redeemerIndex: zero }
      /\ { memory: fromInt' 1685698, steps: fromInt' 609724445 }
  , { redeemerTag: Spend, redeemerIndex: one }
      /\ { memory: fromInt' 1700, steps: fromInt' 476468 }
  ]
