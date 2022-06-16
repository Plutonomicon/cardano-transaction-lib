module Test.Examples.Pkh2Pkh where

import Test.Examples.Config (host)
import Test.E2E.Wallet
import Toppokki as Toki
import TestM
import Mote
import Prelude
import Effect.Aff

js :: String -> Toki.Page -> Aff Unit
js str = void <$> Toki.unsafeEvaluateStringFunction str

x :: Aff Unit
x = do
  page <- launchWithNami Visible >>= Toki.newPage
  namiPage <- Toki.openPopup page (Toki.goto host page)
  js "console.log('test');" namiPage


testPkh2Pkh :: TestPlanM Unit
testPkh2Pkh = test "Pkh2Pkh" x
