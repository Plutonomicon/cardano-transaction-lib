module Test.E2E.Helpers
  ( module E2EHelpers
  , runE2ETest
  , exampleUrl
  , namiSign'
  , geroSign'
  ) where

import Prelude

import Contract.Test.Browser (TestOptions, withBrowser, WalletExt)
import Contract.Test.Feedback (resetTestFeedback)
import Contract.Test.Helpers
  ( RunningExample(RunningExample)
  , WalletPassword
  , checkSuccess
  , delaySec
  , geroSign
  , namiSign
  , withExample
  )
import Contract.Test.Helpers
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
import Control.Monad.Error.Class (try)
import Data.Newtype (wrap, unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Mote (test)
import TestM (TestPlanM)
import Test.Spec.Assertions (shouldSatisfy)
import Toppokki as Toppokki

exampleUrl :: String -> Toppokki.URL
exampleUrl exampleName = wrap $ "http://localhost:4008/?" <> exampleName

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
  \browser -> withExample (exampleUrl example) browser
    ( \e -> do
        liftEffect $ log $ "Start Example " <> example
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
