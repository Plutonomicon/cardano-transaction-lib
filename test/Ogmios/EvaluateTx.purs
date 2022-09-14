module Test.CTL.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch), decodeAeson)
import CTL.Internal.QueryM.Ogmios
  ( ExecutionUnits
  , RedeemerPointer
  , TxEvaluationResult
  )
import CTL.Internal.Types.Natural (fromInt')
import CTL.Internal.Types.RedeemerTag (RedeemerTag(Mint, Spend))
import Data.Either (Either(Left, Right))
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.CTL.Fixtures
  ( ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.CTL.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
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
