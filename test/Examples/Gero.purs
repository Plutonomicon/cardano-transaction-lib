module Test.Examples.Gero (runExample) where

import Prelude

import Data.Newtype (wrap)
import Effect.Aff (delay)
import Mote (test)
import TestM (TestPlanM)
import Test.E2E.Browser (TestOptions, WalletExt(GeroExt))
import Test.E2E.Helpers
  ( checkSuccess
  , geroSign
  , geroConfirmAccess
  , startExample
  , delaySec
  , runE2ETest
  )
import Toppokki as Toppokki

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Gero" options GeroExt geroConfirmAccess

