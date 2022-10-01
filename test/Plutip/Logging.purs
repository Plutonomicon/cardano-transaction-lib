module Test.Plutip.Logging
  ( suite
  ) where

import Prelude

import Contract.Log (logWarn')
import Contract.Test.Plutip (runPlutipContract)
import Ctl.Internal.Test.Utils (TestPlanM)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mote (group, test)
import Test.Plutip.Common (config)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Plutip logging" do
    test "Logs that are not suppressed really appear" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          config
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = false
            }
      runPlutipContract config' unit \_ -> do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
    test "Suppressed logs do not appear when no error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          config
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      runPlutipContract config' unit \_ -> do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` false
    test "Suppressed logs appear on error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          config
            { customLogger = Just
                \_ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      void $ try $ runPlutipContract config' unit \_ -> do
        logWarn' ""
        liftEffect $ throw "Exception"
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
