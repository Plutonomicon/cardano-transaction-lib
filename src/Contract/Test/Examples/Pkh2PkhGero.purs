module Contract.Test.Examples.Pkh2PkhGero (runExample) where

import Prelude

import TestM (TestPlanM)
import Contract.Test.Browser (TestOptions, WalletExt(GeroExt))
import Contract.Test.Examples
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

