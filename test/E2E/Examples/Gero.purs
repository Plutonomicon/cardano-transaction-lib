module Test.E2E.Examples.Gero (runExample) where

import Prelude

import Contract.Test.E2E (TestOptions, WalletExt(GeroExt))
import Test.E2E.Helpers
  ( geroConfirmAccess
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Wallet" options GeroExt geroConfirmAccess

