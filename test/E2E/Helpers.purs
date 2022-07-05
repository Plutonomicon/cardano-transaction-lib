module Test.E2E.Helpers
  ( hasSelector
  , startExample
  , exampleUrl
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
  , showOutput
  , Selector(..)
  , Action(..)
  , NoShowPage(..)
  , ExamplePages(..)
  , E2EOutput
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (head, filterA)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Traversable (for, fold)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1, EffectFn1)
import Foreign (Foreign, typeOf, unsafeFromForeign, unsafeToForeign)
import Toppokki as Toki


import Data.Function.Uncurried as FU

exampleUrl :: Toki.URL
exampleUrl = wrap "http://localhost:4008/"

data OutputType = PageError | Console | RequestFailed

derive instance Eq OutputType

newtype E2EOutput = E2EOutput
                    { outputType :: OutputType
                    , output :: String
                    }

newtype ExamplePages = ExamplePages
  { nami :: Toki.Page
  , main :: Toki.Page
  , errors :: Ref (Array E2EOutput)
  }

derive instance Newtype ExamplePages _

foreign import _retrieveJQuery :: Toki.Page -> Effect (Promise String)

retrieveJQuery :: Toki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

jQueryCount :: Selector -> Toki.Page -> Aff Int
jQueryCount selector page = unsafeFromForeign <$> doJQ selector (wrap "length")
  page

hasSelector :: Selector -> Toki.Page -> Aff Boolean
hasSelector selector page = (_ > 0) <$> jQueryCount selector page

findNamiPage :: String -> Toki.Browser -> Aff (Maybe Toki.Page)
findNamiPage jQuery browser = do
  pages <- injectJQueryAll jQuery browser
  pages' <- filterA (hasSelector button) pages
  pure $ head $ pages'

waitForNamiPage :: String -> Toki.Browser -> Aff Toki.Page
waitForNamiPage jQuery browser =
  findNamiPage jQuery browser >>= case _ of
    Nothing -> do
      delay $ wrap 100.0
      waitForNamiPage jQuery browser
    Just page -> pure page

showOutput :: Ref (Array E2EOutput) -> Effect String
showOutput ref =
  let show' (E2EOutput { outputType, output }) = showType outputType <> " " <> output
      showType PageError     = "ERR"
      showType Console       = "..."
      showType RequestFailed = "REQ"
  in Ref.read ref >>= pure <<< intercalate "\n" <<< map show'

-- | start example at URL with Nami and return both Nami's page and the example's
startExample :: Toki.URL -> Toki.Browser -> Aff ExamplePages
startExample url browser = do
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  errorRef <- liftEffect $ Ref.new []
  liftEffect do
    Toki.onPageError (handler errorRef PageError $ pure <<< show) page
    Toki.onConsole (handler errorRef Console Toki.consoleMessageText) page
    Toki.onRequestFailed (handler errorRef RequestFailed $ pure <<< show) page
  Toki.goto url page
  namiPage <- waitForNamiPage jQuery browser
  pure $ wrap
    { nami: namiPage
    , main: page
    , errors: errorRef
    }
  where
    handler :: forall (a :: Type)
               . Ref (Array (E2EOutput))
               -> OutputType
               -> (a -> Aff String)
               -> EffectFn1 a Unit
    handler errorRef outputType f = mkEffectFn1 $ \e -> launchAff_ do
      output <- f e
      liftEffect $ Ref.modify_ (_ <> [E2EOutput
                                      { outputType : outputType
                                      , output : output
                                      }]) errorRef

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
doJQ :: Selector -> Action -> Toki.Page -> Aff Foreign
doJQ selector action page = do
  Toki.unsafeEvaluateStringFunction jq page
  where
  jq :: String
  jq = "$('" <> unwrap selector <> "')." <> unwrap action

click :: Action
click = wrap "click()"

enable :: Action
enable = wrap "prop(\"disabled\", false)"

setAttr :: String -> String -> Action
setAttr attr value = wrap $ "attr(" <> jqStr attr <> ", " <> value <> ")"

button :: Selector
button = wrap "button"

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
clickButton buttonText = void <$> doJQ (buttonWithText buttonText) click

injectJQueryAll :: String -> Toki.Browser -> Aff (Array Toki.Page)
injectJQueryAll jQuery browser = do
  pages <- Toki.pages browser
  void $ for pages $ \page -> do
    (alreadyInjected :: Boolean) <-
      unsafeFromForeign <$>
        Toki.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'" page
    unless alreadyInjected $ void $ Toki.unsafeEvaluateStringFunction jQuery
      page
  pure pages

testPassword :: String
testPassword = "ctlctlctl"

reactSetValue :: Selector -> String -> Toki.Page -> Aff Unit
reactSetValue selector value page = void
  $ flip Toki.unsafeEvaluateStringFunction page
  $ fold
      [ -- https://stackoverflow.com/questions/23892547/what-is-the-best-way-to-trigger-onchange-event-in-react-js
        -- Nami uses react, which complicates things a bit
        "var input = $('" <> unwrap selector <> "').get(0);"
      , "var nativeInputValueSetter = "
          <>
            " Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;"
      , "nativeInputValueSetter.call(input, '" <> value <> "');"
      , "var ev2 = new Event('input', { bubbles: true});"
      , "input.dispatchEvent(ev2);"
      ]
