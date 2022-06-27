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
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (fst, snd)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
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

findNamiPage :: Toki.Browser -> Aff (Maybe Toki.Page)
findNamiPage browser = do  
  pages <- Toki.pages browser
  results <- traverse (hasSelector $ wrap "button") pages
  pure $ map snd $ head $ filter (isJust <<< fst) $ zip results pages

-- | Wrapper for Page so it can be used in `shouldSatisfy`
newtype NoShowPage = NoShowPage Toki.Page

derive instance Newtype NoShowPage _

instance Show NoShowPage where
  show _ = "Toppoki.Page"

newtype Selector = Selector String
derive instance Newtype Selector _

newtype Action = Action String
derive instance Newtype Action _

doJQ :: Selector -> Action -> Toki.Page -> Aff Unit
doJQ selector action page = do
  log jq
  void $ Toki.unsafeEvaluateStringFunction jq page
  where jq :: String
        jq = "$('" <> unwrap selector <> "')." <> unwrap action

click :: Action
click = wrap "click()"

enable :: Action
enable = wrap "prop(\"disabled\", false)"

setAttr :: String -> String -> Action
setAttr attr value = wrap $ "attr(" <> jqStr attr <> ", " <> value <> ")"

buttonWithText :: String -> Selector
buttonWithText text = wrap $ "button:contains(" <> text <> ")"

password :: Selector
password = wrap ":password"

jqStr :: String -> String
jqStr str = "\"" <> str <> "\""

jqSet :: String -> String -> Action
jqSet what value = wrap $ what <> "(" <> value <> ")"

setText :: String -> Action
setText = jqSet "text" <<< jqStr

setValue :: String -> Action
setValue = jqSet "val" <<< jqStr

trigger :: String -> Action
trigger event = wrap $ "trigger(" <> jqStr event <> ")"

clickButton :: String -> Toki.Page -> Aff Unit
clickButton buttonText = doJQ (buttonWithText buttonText) click

injectJQueryAll :: String -> Toki.Browser -> Aff Unit
injectJQueryAll jQuery browser = do
  pages <- Toki.pages browser
  void $ for pages $ Toki.unsafeEvaluateStringFunction jQuery

testPassword :: String
testPassword = "ctlctlctl"

reactSetValue :: Selector -> String -> Toki.Page -> Aff Unit
reactSetValue selector value page = void $ flip Toki.unsafeEvaluateStringFunction page $ fold
                     [ -- https://stackoverflow.com/questions/23892547/what-is-the-best-way-to-trigger-onchange-event-in-react-js
                       -- Nami uses react, which complicates things a bit
                       "var input = $('" <> unwrap selector <> "').get(0);"
                     , "var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;"
                     , "nativeInputValueSetter.call(input, '" <> value <> "');"
                     , "var ev2 = new Event('input', { bubbles: true});"
                     , "input.dispatchEvent(ev2);"
                     ]

x :: Aff Unit
x = do
  browser <- launchWithNami Visible
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  Toki.goto (wrap example) page
  delay (wrap 5000.0)
  injectJQueryAll jQuery browser
  namiPage <- findNamiPage browser
  shouldSatisfy (NoShowPage <$> namiPage) isJust
  case namiPage of
    Nothing -> liftEffect $ throw "Impossible"
    Just np -> do clickButton "Access" np -- this matches when the wallet is used the first time
                  clickButton "Sign" np
                  reactSetValue password testPassword np
                  clickButton "Confirm" np
  delay (wrap 600000.0)
  
{-  let namiPage' :: Toki.Page
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
