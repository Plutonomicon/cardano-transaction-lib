-- | Augmented version of Contract.Test.E2E.Helpers, with some functions that
-- | are only useful for testing CTL itself.
module Test.E2E.Helpers
  ( module E2EHelpers
  , runE2ETest
  , exampleUrl
  , namiSign'
  , geroSign'
  ) where

import Prelude

import Contract.Test.E2E
  ( E2EOutput
  , RunningExample(RunningExample)
  , WalletPassword(WalletPassword)
  , checkSuccess
  , delaySec
  , geroConfirmAccess
  , geroSign
  , namiConfirmAccess
  , namiSign
  , withExample
  ) as E2EHelpers
import Contract.Test.E2E
  ( RunningExample
  , TestOptions
  , WalletExt(NamiExt, GeroExt)
  , WalletPassword
  , checkSuccess
  , delaySec
  , geroSign
  , namiSign
  , resetTestFeedback
  , withBrowser
  , withExample
  )
import Control.Monad.Error.Class (try)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mote (test)
import Test.Spec.Assertions (shouldSatisfy)
import TestM (TestPlanM)
import Toppokki as Toppokki

walletName :: WalletExt -> String
walletName NamiExt = "nami"
walletName GeroExt = "gero"

exampleUrl :: String -> WalletExt -> Toppokki.URL
exampleUrl exampleName wallet = wrap $ "http://localhost:4008/?" <> exampleName
  <> ":"
  <> walletName wallet

testPasswordNami :: WalletPassword
testPasswordNami = wrap "ctlctlctl"

testPasswordGero :: WalletPassword
testPasswordGero = wrap "VZVfu5rp1r"

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
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \browser -> withExample (exampleUrl example ext) browser
    ( \e -> do
        liftEffect $ log $ "Start Example " <> example <> ":" <> walletName ext
        resetTestFeedback (_.main $ unwrap e)
        void $ try $ f e
        delaySec 10.0
        liftEffect $ log $ "Example " <> example <>
          " finished, check success..."
        checkSuccess e >>= flip shouldSatisfy (_ == true)
    )

namiSign' :: RunningExample -> Aff Unit
namiSign' = namiSign testPasswordNami

geroSign' :: RunningExample -> Aff Unit
geroSign' = geroSign testPasswordGero
