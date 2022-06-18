module Test.Examples.Pkh2Pkh where

import Test.Examples.Config (host)
import Test.E2E.Wallet
import Toppokki as Toki
import TestM
import Mote
import Prelude
import Effect.Aff
import Data.Traversable
import Test.Toppoki (example)
import Test.Spec.Assertions (shouldSatisfy)
import Data.Newtype (wrap)
import Foreign (Foreign)
import Effect.Class.Console (log)
import Control.Monad.Error.Class (catchError)
import Control.Category (identity)

js :: String -> Toki.Page -> Aff Unit
js str = void <$> Toki.unsafeEvaluateStringFunction str

hasButton :: Toki.Page -> Aff Boolean
hasButton page = catchError hasButton' $ \e -> pure false
  where hasButton' :: Aff Boolean
        hasButton' = do
          _ <- Toki.unsafePageEval (wrap "button") "(x)=>{}" page 
          pure true

x :: Aff Unit
x = do
  browser <- launchWithNami Headless
  page <- Toki.newPage browser
  Toki.goto (wrap example) page
  delay (wrap 5000.0)  
  pages <- Toki.pages browser
  results <- traverse hasButton pages :: Aff (Array Boolean)
  shouldSatisfy results (any identity)
--  namiPage <- Toki.openPopup page (Toki.goto host page)
--  js "console.log('test');" namiPage


testPkh2Pkh :: TestPlanM Unit
testPkh2Pkh = test "Pkh2Pkh" x
