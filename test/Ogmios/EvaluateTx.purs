module Test.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch), decodeAeson)
import Data.Either (Either(Left, Right))
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Mote (group, test)
import QueryM.Ogmios (ExecutionUnits, RedeemerPointer, TxEvaluationResult)
import Test.Fixtures
  ( ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.Natural (fromInt')
import Types.RedeemerTag (RedeemerTag(Mint, Spend))

suite :: TestPlanM Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either JsonDecodeError TxEvaluationResult <-
          decodeAeson <$> liftEffect ogmiosEvaluateTxValidRespFixture
        (Map.toUnfoldable <<< unwrap <$> txEvalR) `shouldEqual`
          Right ogmiosEvaluateTxValidRespDecoded

      test "Fails to decode a response with invalid redeemer pointer format" do
        txEvalR :: Either JsonDecodeError TxEvaluationResult <-
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
