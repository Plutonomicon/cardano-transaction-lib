module Test.Examples.Pkh2Pkh (runExample) where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (delay)
import Mote (test)
import Test.E2E.Browser (TestOptions, WalletExt(NamiExt))
import Test.E2E.Helpers
  ( checkSuccess
  , namiSign
  , namiConfirmAccess
  , startExample
  , delaySec
  , runE2ETest
  )
import TestM (TestPlanM)
import Toppokki as Toppokki
import Effect.Console (log)
import Effect.Class (liftEffect)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Pkh2Pkh" options NamiExt $ \example -> do
  namiConfirmAccess example
  namiSign example
