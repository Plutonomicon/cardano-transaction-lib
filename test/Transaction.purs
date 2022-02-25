module Test.Transaction (suite) where

import Prelude

import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Deserialization.PlutusData as Deserialization.PlutusData
import Serialization.WitnessSet as Serialization.WitnessSet
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)
import TestM (TestPlanM)
import Transaction (attachDatum)
import Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  )
import Types.PlutusData (Datum(Datum), PlutusData(Integer))

suite :: TestPlanM Unit
suite = group "attach datums to tx"
  $ test "datum should be correctly attached" testAttachDatum

testAttachDatum :: Aff Unit
testAttachDatum = liftEffect $
  attachDatum datum tx >>= case _ of
    Left e -> throw $ "Failed to attach datum: " <> show e
    Right (Transaction { witness_set: TransactionWitnessSet ws }) ->
      case ws.plutus_data of
        Just [ pd ] -> do
          pd' <- Deserialization.PlutusData.convertPlutusData
            <$> (Serialization.WitnessSet.convertPlutusData pd)
          pd' `shouldEqual` Just (unwrap datum)
        Just _ -> throw "Incorrect number of datums attached"
        Nothing -> throw "Datum wasn't attached"
  where
  tx :: Transaction
  tx = mempty

  datum :: Datum
  datum = Datum $ Integer $ BigInt.fromInt 1
