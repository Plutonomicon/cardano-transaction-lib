module Test.E2E.Examples.AlwaysMints (runExample) where

import Prelude

import Contract.Test.E2E (TestOptions, SomeWallet(SomeWallet), WalletExt(NamiExt), WalletPassword)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Test.E2E.Helpers
  ( delaySec
  , runE2ETest
  )
import TestM (TestPlanM)

runExample :: SomeWallet -> WalletPassword -> TestOptions -> TestPlanM (Aff Unit) Unit
runExample (SomeWallet {wallet, confirmAccess, sign}) password options = runE2ETest "AlwaysMints" options wallet $ \example -> do
  confirmAccess example
  delaySec 3.0
  sign password example
