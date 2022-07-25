{- | This module is intended to be used for running custom E2E-tests -}
module Contract.Test.Examples
  ( module E2EHelpers
  ) where

import Test.E2E.Helpers
  ( E2EOutput
  , RunningExample(RunningExample)
  , WalletPassword
  , delaySec
  , namiConfirmAccess
  , namiSign
  , geroConfirmAccess
  , geroSign
  , withExample
  ) as E2EHelpers
