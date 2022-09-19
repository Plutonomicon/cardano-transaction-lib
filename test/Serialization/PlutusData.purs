module Test.Serialization.PlutusData (suite) where

import Prelude (not, Unit, ($))

import Types.PlutusData as T
import Serialization.PlutusData (convertPlutusData)
import Data.Maybe (Maybe(Just), isNothing, isJust)
import Data.Eq ((==))
import Effect.Aff (Aff)

import Test.Utils (assertTrue)
import TestM (TestPlanM)
import Test.Spec.Assertions (shouldSatisfy)
import Data.BigInt as BigInt

-- test convertPlutusData

--convertPlutusData :: T.PlutusData -> Maybe PlutusData
--convertPlutusData = case _ of
--T.Constr alt list -> convertConstr alt list
--T.Map mp -> convertPlutusMap mp
--T.List lst -> convertPlutusList lst
--T.Integer n -> convertPlutusInteger n
--T.Bytes b -> pure $ _mkPlutusData_bytes b

suite :: TestPlanM (Aff Unit) Unit
suite = do
  let bigInt = BigInt.fromInt 2147483647
  assertTrue "isJust"
    (isJust $ convertPlutusData (T.Integer bigInt))
