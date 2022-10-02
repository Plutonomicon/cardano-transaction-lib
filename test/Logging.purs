module Test.Ctl.Logging (suite) where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Log (logWarn')
import Contract.Monad (runContract)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mote (group, test)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Logging" do
    test "Logs that are not suppressed really appear" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          testnetConfig
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = false
            }
      runContract config' do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
    test "Suppressed logs do not appear when no error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          testnetConfig
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      runContract config' do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` false
    test "Suppressed logs appear on error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          testnetConfig
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      void $ try $ runContract config' do
        logWarn' ""
        liftEffect $ throw "Exception"
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
