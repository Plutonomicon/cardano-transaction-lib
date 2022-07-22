module Contract.Test.Examples.Gero (runExample) where

import Prelude

import TestM (TestPlanM)
import Contract.Test.Browser (TestOptions, WalletExt(GeroExt))
import Contract.Test.Examples
  ( geroConfirmAccess
  , runE2ETest
  )

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "Gero" options GeroExt geroConfirmAccess

