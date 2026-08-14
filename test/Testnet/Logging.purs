module Test.Ctl.Testnet.Logging
  ( suite
  ) where

import Prelude

import Contract.Log (logWarn')
import Contract.Test.Testnet (defaultTestnetConfig, runTestnetContract)
import Data.Log.Level (LogLevel(Error))
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Logging" do
    test "Logs that are not suppressed really appear" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          defaultTestnetConfig
            { customLogger = Just
                \_ _ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = false
            }
      runTestnetContract config' unit \_ -> do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
    test "Suppressed logs do not appear when no error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          defaultTestnetConfig
            { customLogger = Just
                \_ _ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      runTestnetContract config' unit \_ -> do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` false
    test "Suppressed logs appear on error" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          defaultTestnetConfig
            { customLogger = Just
                \_ _ -> liftEffect $ Ref.write true hasLogged
            , suppressLogs = true
            }
      void $ try $ runTestnetContract config' unit \_ -> do
        logWarn' ""
        liftEffect $ throw "Exception"
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` true
    test "CustomLogger, filtered by LogLevel, does not log" do
      hasLogged <- liftEffect $ Ref.new false
      let
        config' =
          defaultTestnetConfig
            { customLogger = Just writeLog
            , suppressLogs = false
            , logLevel = Error
            }
        writeLog lgl m = liftEffect $ when (m.level >= lgl) $ do
          Ref.write true hasLogged
      runTestnetContract config' unit \_ -> do
        logWarn' ""
      hasLoggedResult <- liftEffect $ Ref.read hasLogged
      hasLoggedResult `shouldEqual` false
