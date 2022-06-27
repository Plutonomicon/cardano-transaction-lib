module Test.E2E.Helpers
  ( js
  , tryJs
  , hasSelector
  , injectJQuery
  , injectJQueryAll
  , findNamiPage
  , doJQ
  , click
  , enable
  , setAttr
  , buttonWithText
  , password
  , jqStr
  , jqSet
  , setText
  , setValue
  , trigger
  , clickButton
  , testPassword
  , reactSetValue
  , retrieveJQuery
  , Selector(..)
  , Action(..)
  , NoShowPage(..)
  ) where

import Toppokki as Toki
import Control.Monad.Error.Class (catchError)
import Data.Array (head, filter, zip)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect (Effect)
import Data.Maybe (Maybe(..), isJust)
import Foreign (Foreign)
import Prelude
import Control.Promise (Promise, toAffE)
import Data.Traversable (for, traverse, fold)

foreign import _retrieveJQuery :: Toki.Page -> Effect (Promise String)

retrieveJQuery :: Toki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

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

injectJQuery :: String -> Toki.Page -> Aff Foreign
injectJQuery = Toki.unsafeEvaluateStringFunction

findNamiPage :: Toki.Browser -> Aff (Maybe Toki.Page)
findNamiPage browser = do
  pages <- Toki.pages browser
  results <- traverse (hasSelector $ wrap "button") pages
  pure $ map snd $ head $ filter (isJust <<< fst) $ zip results pages

-- | Wrapper for Page so it can be used in `shouldSatisfy`, which needs 'Show'
-- | Doesn't show anything, thus 'NoShow'
newtype NoShowPage = NoShowPage Toki.Page

derive instance Newtype NoShowPage _

instance Show NoShowPage where
  show _ = "Toppoki.Page"

newtype Selector = Selector String

derive instance Newtype Selector _

newtype Action = Action String

derive instance Newtype Action _

-- | Build a primitive jQuery expression like '$("button").click()' and evaluate it in Toki
doJQ :: Selector -> Action -> Toki.Page -> Aff Unit
doJQ selector action page = do
  log jq
  void $ Toki.unsafeEvaluateStringFunction jq page
  where
  jq :: String
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
reactSetValue selector value page = void
  $ flip Toki.unsafeEvaluateStringFunction page
  $ fold
      [ -- https://stackoverflow.com/questions/23892547/what-is-the-best-way-to-trigger-onchange-event-in-react-js
        -- Nami uses react, which complicates things a bit
        "var input = $('" <> unwrap selector <> "').get(0);"
      , "var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;"
      , "nativeInputValueSetter.call(input, '" <> value <> "');"
      , "var ev2 = new Event('input', { bubbles: true});"
      , "input.dispatchEvent(ev2);"
      ]
