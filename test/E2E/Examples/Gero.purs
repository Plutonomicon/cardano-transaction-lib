module Test.E2E.Examples.Gero (runExample) where

import Prelude

import TestM (TestPlanM)
import Contract.Test.E2E (TestOptions, WalletExt(GeroExt))
import Effect.Aff (Aff)
import Test.E2E.Helpers
  ( geroConfirmAccess
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM (Aff Unit) Unit
runExample options = runE2ETest "Wallet" options GeroExt geroConfirmAccess

