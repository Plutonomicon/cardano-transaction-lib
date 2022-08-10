module Test.E2E.Examples.MintsMultipleTokens (runExample) where

import Prelude

import Contract.Test.E2E
  ( TestOptions
  , WalletExt(NamiExt)
  )
import Test.E2E.Helpers
  ( namiSign'
  , namiConfirmAccess
  , runE2ETest
  )

import TestM (TestPlanM)

runExample :: TestOptions -> TestPlanM Unit
runExample options = runE2ETest "MintsMultipleTokens" options NamiExt $
  \example -> do
    namiConfirmAccess example
    namiSign' example
