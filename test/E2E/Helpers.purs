-- | Augmented version of Contract.Test.E2E.Helpers, with some functions that
-- | are only useful for testing CTL itself.
module Test.E2E.Helpers where

import Prelude

-- import Contract.Test.E2E
--   ( RunningExample
--   , SomeWallet
--   , WalletPassword(WalletPassword)
--   , delaySec
--   , geroConfirmAccess
--   , geroSign
--   , namiConfirmAccess
--   , namiSign
--   , withExample
--   ) as E2EHelpers
-- import Contract.Test.E2E
--   ( RunningExample
--   , TestOptions
--   , WalletExt
--   , delaySec
--   , walletName
--   , withBrowser
--   , withExample
--   )
-- import Data.Maybe (Maybe(Just, Nothing))
-- import Data.Newtype (wrap)
-- import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Console (log)
-- import Mote (test)
-- import Test.Spec.Assertions (shouldSatisfy)
-- import TestM (TestPlanM)
-- import Toppokki as Toppokki

-- -- TODO: delete?
-- exampleUrl :: String -> WalletExt -> Toppokki.URL
-- exampleUrl exampleName wallet = wrap $ "http://localhost:4008/?" <> exampleName
--   <> ":"
--   <> walletName wallet
