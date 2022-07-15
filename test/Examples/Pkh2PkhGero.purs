module Test.Examples.Pkh2PkhGero (runExample) where

import Prelude

import TestM (TestPlanM)
import Test.E2E.Browser (TestOptions, WalletExt(GeroExt))
import Test.E2E.Helpers
  ( geroSign
  , geroConfirmAccess
  , delaySec
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Pkh2PkhGero" options GeroExt $ \example -> do
  geroConfirmAccess example
  delaySec 7.0
  geroSign example

