module Test.Examples.Pkh2Pkh where

import Control.Monad.Maybe.Trans
import Data.Traversable
import Debug
import Effect.Aff
import Mote
import Prelude
import Test.E2E.Wallet
import TestM

import Control.Category (identity)
import Control.Monad.Error.Class (catchError)
import Data.Array (head, filter, zip)
import Data.Functor (void)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Tuple (fst, snd)
import Effect.Class.Console (log)
import Foreign (Foreign)
import Foreign as Foreign
import Test.E2E.Helpers (retrieveJQuery)
import Test.Examples.Config (host)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Toppoki (example)
import Toppokki as Toki

js :: String -> Toki.Page -> Aff Unit
js str = void <$> Toki.unsafeEvaluateStringFunction str

tryJs :: Toki.Selector -> String -> Toki.Page -> Aff (Maybe Unit)
tryJs selector function page = catchError eval $ \e -> do
                                                       log $ show e
                                                       pure Nothing
  where
  eval :: Aff (Maybe Unit)
  eval = do
    _ <- Toki.unsafePageEval selector function page
    pure $ Just unit

hasSelector :: Toki.Selector -> Toki.Page -> Aff (Maybe Unit)
hasSelector selector page = tryJs selector "(x)=>{}" page

{-retrieveJQuery :: Toki.Page -> Aff String
retrieveJQuery page = Foreign.unsafeFromForeign <$> Toki.unsafeEvaluateStringFunction fetch page
  where fetch = "window.fetch('https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js')"
-}

injectJQuery :: String -> Toki.Page -> Aff Foreign
injectJQuery = Toki.unsafeEvaluateStringFunction

findNamiPage :: Toki.Page -> Toki.Browser -> Aff (Maybe Toki.Page)
findNamiPage page browser = do
  
  pages <- Toki.pages browser
  _ <- pure $ spy "retrieve it!"
  jQuery <- retrieveJQuery page
  _ <- for pages $ injectJQuery jQuery
  
  _ <- pure $ spy "ohno" true
  results <- traverse (hasSelector $ wrap "button") pages
  _ <- pure $ spy "Results" results  
  let namiPage = map snd $ head $ filter (isJust <<< fst) $ zip results pages
  case namiPage of
      Nothing -> pure Nothing
      Just np -> debugger $ \_ -> do
        Toki.addScriptTag "https://code.jquery.com/jquery-3.2.1.min.js" np
        _ <- pure $ spy "added Scripttag " true
        pure namiPage

-- | Wrapper for Page so it can be used in `shouldSatisfy`
newtype NoShowPage = NoShowPage Toki.Page

derive instance Newtype NoShowPage _

instance Show NoShowPage where
  show _ = "Toppoki.Page"

clickButton :: String -> Toki.Page -> Aff (Maybe Unit)
clickButton buttonText page = do
  let selector = "button"
  log selector
  tryJs (wrap selector) "(b) => b.click()" page

x :: Aff Unit
x = do
  browser <- launchWithNami Visible
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  Toki.goto (wrap example) page
  delay (wrap 5000.0)
{-  namiPage <- findNamiPage page browser
  shouldSatisfy (NoShowPage <$> namiPage) isJust
  let namiPage' :: Toki.Page
      namiPage' = fromMaybe page namiPage
  r <- clickButton "Sign" namiPage'
  shouldSatisfy r (_ == (Just unit))
  delay (wrap 60000.0)
  pure unit
-}
--  namiPage <- Toki.openPopup page (Toki.goto host page)
--  js "console.log('test');" namiPage

testPkh2Pkh :: TestPlanM Unit
testPkh2Pkh = test "Pkh2Pkh" x
