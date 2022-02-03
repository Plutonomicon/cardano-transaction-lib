module Test.Deserialization where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect.Class (liftEffect)
import Mote (group, test)
import Serialization as Serialization
import Deserialization as Deserialization
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import TestM (TestPlanM)
import Types.Transaction as T
import Effect.Exception (throw)

suite :: TestPlanM Unit
suite = do
  group "cardano-serialization-lib bindings (deserialization)" $ do
    group "Address" $ do
      test "deserialization works" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString)
        Deserialization.convertAddress address `shouldSatisfy` isJust
      test "deserialization is inverse to serialization" do
        address <- liftEffect $ Serialization.newAddressFromBech32 (T.Bech32 addressString)
        liftEffect case Deserialization.convertAddress address of
          Nothing -> throw "Failed deserialization"
          Just address' -> do
            address'' <- Serialization.convertAddress address'
            case Deserialization.convertAddress address'' of
              Nothing -> throw "Failed deserialization"
              Just address''' -> do
                address''' `shouldEqual` address'

addressString :: String
addressString = "addr1qyc0kwu98x23ufhsxjgs5k3h7gktn8v5682qna5amwh2juguztcrc8hjay66es67ctn0jmr9plfmlw37je2s2px4xdssgvxerq"
