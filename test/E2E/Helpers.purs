-- | Augmented version of Contract.Test.E2E.Helpers, with some functions that
-- | are only useful for testing CTL itself.
module Test.CTL.E2E.Helpers
  ( module E2EHelpers
  , runE2ETest
  , exampleUrl
  ) where

import Prelude

import CTL.Contract.Test.E2E
  ( E2EOutput
  , RunningExample
  , SomeWallet
  , WalletPassword(WalletPassword)
  , checkSuccess
  , delaySec
  , geroConfirmAccess
  , geroSign
  , namiConfirmAccess
  , namiSign
  , withExample
  ) as E2EHelpers
import CTL.Contract.Test.E2E
  ( RunningExample
  , TestOptions
  , WalletExt
  , checkSuccess
  , delaySec
  , resetTestFeedback
  , walletName
  , withBrowser
  , withExample
  )
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mote (test)
import Test.CTL.TestM (TestPlanM)
import Test.Spec.Assertions (shouldSatisfy)
import Toppokki as Toppokki

exampleUrl :: String -> WalletExt -> Toppokki.URL
exampleUrl exampleName wallet = wrap $ "http://localhost:4008/?" <> exampleName
  <> ":"
  <> walletName wallet

-- | Run an E2E test. Parameters are:
-- |   String: Just a name for the logs
-- |   Toppokki.URL: URL where the example is running
-- |   TestOptions: Options to start the browser with
-- |   WalletExt: An extension which should be used
-- |   RunningExample -> Aff a: A function which runs the test
runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM (Aff Unit) Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \mbrowser ->
    case mbrowser of
      Nothing -> do
        liftEffect $ log $ "Wallet " <> walletName ext <> " not provided"
      Just browser ->
        withExample (exampleUrl example ext) browser
          ( \e -> do
              liftEffect $ log $ "Start Example " <> example <> ":" <>
                walletName ext
              resetTestFeedback (_.main e)
              void $ f e
              delaySec 30.0
              liftEffect $ log $ "Example " <> example <>
                " finished, check success..."
              checkSuccess e >>= flip shouldSatisfy (_ == true)
          )
