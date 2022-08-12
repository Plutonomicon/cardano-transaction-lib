module Test.E2E.Examples.Pkh2PkhGero (runExample) where

import Prelude

import TestM (TestPlanM)
import Contract.Test.E2E (TestOptions, WalletExt(GeroExt))
import Test.E2E.Helpers
  ( geroSign'
  , geroConfirmAccess
  , delaySec
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Pkh2Pkh" options GeroExt $ \example -> do
  geroConfirmAccess example
  delaySec 7.0
  geroSign' example

