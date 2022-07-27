module Test.E2E.Examples.Gero (runExample) where

import Prelude

import TestM (TestPlanM)
import Contract.Test.E2E (TestOptions, WalletExt(GeroExt))
import Test.E2E.Helpers
  ( geroConfirmAccess
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Gero" options GeroExt geroConfirmAccess

