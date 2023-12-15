module Test.Ctl.Ogmios.EvaluateTx (suite) where

import Prelude

import Aeson (JsonDecodeError(TypeMismatch))
import Contract.Numeric.Natural (Natural, fromBigInt')
import Ctl.Internal.QueryM.JsonRpc2
  ( OgmiosDecodeError(ResultDecodingError)
  , decodeOgmios
  )
import Ctl.Internal.QueryM.Ogmios
  ( ExecutionUnits
  , RedeemerPointer
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , TxEvaluationR(TxEvaluationR)
  , TxEvaluationResult(TxEvaluationResult)
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Types.RedeemerTag (RedeemerTag(Spend, Cert, Reward))
import Data.Either (Either(Left, Right))
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JS.BigInt as BigInt
import Mote (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures
  ( ogmiosEvaluateTxFailIncompatibleEraFixture
  , ogmiosEvaluateTxFailScriptErrorsFixture
  , ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          decodeOgmios <$> liftEffect ogmiosEvaluateTxValidRespFixture
        txEvalR `shouldSatisfy` case _ of
          Right (TxEvaluationR (Right (TxEvaluationResult map))) ->
            Map.toUnfoldable map ==
              ogmiosEvaluateTxValidRespDecoded
          _ -> false

      test "Fails to decode a response with invalid redeemer pointer format" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          decodeOgmios <$> liftEffect
            ogmiosEvaluateTxInvalidPointerFormatFixture
        txEvalR `shouldSatisfy` case _ of
          Left (ResultDecodingError (TypeMismatch _)) -> true
          _ -> false

      test "Successfully decodes a failed execution response (Incompatible era)"
        do
          txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
            decodeOgmios <$> liftEffect
              ogmiosEvaluateTxFailIncompatibleEraFixture
          txEvalR `shouldSatisfy` case _ of
            Right (TxEvaluationR (Left (UnparsedError _))) -> true
            _ -> false

      test "Successfully decodes a failed execution response (Script errors)" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          decodeOgmios <$> liftEffect
            ogmiosEvaluateTxFailScriptErrorsFixture
        txEvalR `shouldSatisfy` case _ of
          Right (TxEvaluationR (Left (ScriptFailures _))) -> true
          _ -> false

ogmiosEvaluateTxValidRespDecoded :: Array (RedeemerPointer /\ ExecutionUnits)
ogmiosEvaluateTxValidRespDecoded = Map.toUnfoldable $ Map.fromFoldable
  [ { redeemerTag: Cert, redeemerIndex: one + one + one }
      /\
        { memory: naturalLiteral "4926587050210136942"
        , steps: naturalLiteral "2982577810151428748"
        }
  , { redeemerTag: Spend, redeemerIndex: one }
      /\
        { memory: naturalLiteral "2766916028110716146"
        , steps: naturalLiteral "6325731070934221229"
        }
  , { redeemerTag: Reward, redeemerIndex: zero }
      /\
        { memory: naturalLiteral "3603965291794951667"
        , steps: naturalLiteral "937555587227912939"
        }
  ]

naturalLiteral :: String -> Natural
naturalLiteral x = fromBigInt' $ unsafePartial $ fromJust $ BigInt.fromString x
