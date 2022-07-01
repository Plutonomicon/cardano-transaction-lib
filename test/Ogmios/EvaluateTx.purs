module Test.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (decodeAeson, JsonDecodeError(TypeMismatch))
import Data.BigInt (fromInt) as BigInt
import Data.Either (Either(Left, Right))
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Mote (group, test)
import Test.Fixtures
  ( ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.Natural (Natural)
import Types.Natural (fromBigInt') as Natural
import Types.RedeemerTag (RedeemerTag(Mint, Spend))
import QueryM.Ogmios (TxEvaluationR, ExecutionUnits, RedeemerPointer)

suite :: TestPlanM Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either JsonDecodeError TxEvaluationR <-
          decodeAeson <$> liftEffect ogmiosEvaluateTxValidRespFixture
        (Map.toUnfoldable <<< unwrap <$> txEvalR) `shouldEqual`
          Right ogmiosEvaluateTxValidRespDecoded

      test "Fails to decode a response with invalid redeemer pointer format" do
        txEvalR :: Either JsonDecodeError TxEvaluationR <-
          decodeAeson <$> liftEffect ogmiosEvaluateTxInvalidPointerFormatFixture
        txEvalR `shouldSatisfy` case _ of
          Left (TypeMismatch _) -> true
          _ -> false

ogmiosEvaluateTxValidRespDecoded :: Array (RedeemerPointer /\ ExecutionUnits)
ogmiosEvaluateTxValidRespDecoded =
  [ { redeemerTag: Mint, redeemerIndex: nat zero }
      /\ { memory: nat 1685698, steps: nat 609724445 }
  , { redeemerTag: Spend, redeemerIndex: nat one }
      /\ { memory: nat 1700, steps: nat 476468 }
  ]

nat :: Int -> Natural
nat = Natural.fromBigInt' <<< BigInt.fromInt
