module Test.E2E.Helpers
  ( hasSelector
  , startExample
  , exampleUrl
  , doJQ
  , click
  , checkSuccess
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
  , inWalletPage
  , namiConfirmAccess
  , namiSign
  , geroConfirmAccess
  , geroSign
  , geroInit
  , Selector(..)
  , Action(..)
  , NoShowPage(..)
  , RunningExample(..)
  , E2EOutput
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (head, filterA)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Traversable (for, fold)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1, EffectFn1)
import Foreign (Foreign, unsafeFromForeign)
import Test.E2E.Feedback (testFeedbackIsTrue)
import Test.Spec.Assertions (shouldSatisfy)
import Toppokki as Toki

exampleUrl :: Toki.URL
exampleUrl = wrap "http://localhost:4008/"

testPassword :: String
testPassword = "ctlctlctl"

testPasswordGero :: String
testPasswordGero = "VZVfu5rp1r"

geroSeed :: String
geroSeed =
  "blanket source govern pony crash genius army announce space topic fan beauty clever risk act"

data OutputType = PageError | Console | RequestFailed

derive instance Eq OutputType

newtype E2EOutput = E2EOutput
  { outputType :: OutputType
  , output :: String
  }

newtype RunningExample = RunningExample
  { browser :: Toki.Browser
  , jQuery :: String
  , main :: Toki.Page
  , errors :: Ref (Array E2EOutput)
  }

derive instance Newtype RunningExample _

retrieveJQuery :: Toki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

typeInto :: Selector -> String -> Toki.Page -> Aff Unit
typeInto selector text page = toAffE $ _typeInto selector text page

jQueryCount :: Selector -> Toki.Page -> Aff Int
jQueryCount selector page = unsafeFromForeign <$> doJQ selector (wrap "length")
  page

hasSelector :: Selector -> Toki.Page -> Aff Boolean
hasSelector selector page = (_ > 0) <$> jQueryCount selector page

findWalletPage :: String -> Toki.Browser -> Aff (Maybe Toki.Page)
findWalletPage jQuery browser = do
  pages <- injectJQueryAll jQuery browser
  pages' <- filterA (hasSelector button) pages
  pure $ head $ pages'

waitForWalletPage :: String -> Toki.Browser -> Aff Toki.Page
waitForWalletPage jQuery browser =
  findWalletPage jQuery browser >>= case _ of
    Nothing -> do
      delay $ wrap 100.0
      waitForWalletPage jQuery browser
    Just page -> pure page

showOutput :: Ref (Array E2EOutput) -> Effect String
showOutput ref =
  let
    show' (E2EOutput { outputType, output }) = showType outputType <> " " <>
      output
    showType PageError = "ERR"
    showType Console = "..."
    showType RequestFailed = "REQ"
  in
    Ref.read ref >>= pure <<< intercalate "\n" <<< map show'

-- | start example at URL with Nami and return both Nami's page and the example's
startExample :: Toki.URL -> Toki.Browser -> Aff RunningExample
startExample url browser = do
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  errorRef <- liftEffect $ Ref.new []
  liftEffect do
    Toki.onPageError (handler errorRef PageError $ pure <<< show) page
    Toki.onConsole (handler errorRef Console Toki.consoleMessageText) page
    Toki.onRequestFailed (handler errorRef RequestFailed $ pure <<< show) page
  Toki.goto url page
  pure $ wrap
    { browser: browser
    , jQuery: jQuery
    , main: page
    , errors: errorRef
    }
  where
  handler
    :: forall (a :: Type)
     . Ref (Array (E2EOutput))
    -> OutputType
    -> (a -> Aff String)
    -> EffectFn1 a Unit
  handler errorRef outputType f = mkEffectFn1 $ \e -> launchAff_ do
    output <- f e
    liftEffect $ Ref.modify_
      ( _ <>
          [ E2EOutput
              { outputType: outputType
              , output: output
              }
          ]
      )
      errorRef

checkSuccess :: RunningExample -> Aff Unit
checkSuccess (RunningExample { main, errors }) = do
  feedback <- testFeedbackIsTrue main
  unless feedback $ liftEffect $ showOutput errors >>= log
  shouldSatisfy feedback (_ == true)

inWalletPage
  :: forall (a :: Type). RunningExample -> (Toki.Page -> Aff a) -> Aff a
inWalletPage (RunningExample { browser, jQuery }) =
  (waitForWalletPage jQuery browser >>= _)

namiConfirmAccess :: RunningExample -> Aff Unit
namiConfirmAccess = flip inWalletPage (clickButton "Access")

namiSign :: RunningExample -> Aff Unit
namiSign = flip inWalletPage \nami -> do
  clickButton "Sign" nami
  reactSetValue password testPassword nami
  clickButton "Confirm" nami

geroConfirmAccess :: RunningExample -> Aff Unit
geroConfirmAccess =
  flip inWalletPage $ \page -> do
    delay $ wrap 100.0
    void $ doJQ (inputType "radio") click page
    void $ doJQ (buttonWithText "Continue") click page
    delay $ wrap 100.0
    void $ doJQ (inputType "checkbox") click page
    void $ doJQ (buttonWithText "Connect") click page

geroInit :: Toki.Browser -> Aff Unit
geroInit browser = do
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  Toki.goto
    (wrap "chrome-extension://iifeegfcfhlhhnilhfoeihllenamcfgc/index.html")
    page
  void $ Toki.unsafeEvaluateStringFunction jQuery page
  delay $ wrap 1000.0
  void $ doJQ (buttonWithText "Get Started") click page
  delay $ wrap 1000.0
  void $ doJQ' (wrap "a[href=\"#/wallet-options/import-wallet\"]") click page
  delay $ wrap 100.0
  reactSetValue (byId "wallet-name") "ctltest" page
  typeInto textarea geroSeed page
  void $ doJQ (buttonWithText "Import") click page
  delay $ wrap 100.0
  typeInto (byId "wallet-password") testPasswordGero page
  typeInto (byId "wallet-password-confirm") testPasswordGero page
  void $ doJQ (byId "user-agree") click page
  void $ doJQ (byId "password-agree") click page
  void $ doJQ (byId "set-up-password") click page
  delay $ wrap 100.0
  void $ doJQ (buttonWithText "Continue") click page
  delay $ wrap 100.0
  void $ doJQ (byId "done") click page
  openPopup browser
  delay $ wrap 50000.0
  pure unit

{-geroSetCollateral :: Toki.Browser -> Aff Unit
geroSetCollateral = do
  page <- Toki.newPage browser
  jQuery <- retrieveJQuery page
  Toki.goto (wrap "chrome-extension://iifeegfcfhlhhnilhfoeihllenamcfgc/index.html") page
-}

geroSign :: RunningExample -> Aff Unit
geroSign (RunningExample { browser }) = do
  pure unit

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
doJQ selector = doJQInd selector Nothing

doJQ' :: Selector -> Action -> Toki.Page -> Aff Foreign
doJQ' selector = doJQInd selector (Just 0)

doJQInd :: Selector -> Maybe Int -> Action -> Toki.Page -> Aff Foreign
doJQInd selector index action page = do
  liftEffect $ log jq
  Toki.unsafeEvaluateStringFunction jq page
  where
  jq :: String
  jq = "$('" <> unwrap selector <> "')" <> indexStr <> "." <> unwrap action

  indexStr :: String
  indexStr = maybe "" (\i -> "[" <> show i <> "]") index

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

inputType :: String -> Selector
inputType typ = wrap $ "input[type=\"" <> typ <> "\"]"

textarea :: Selector
textarea = wrap "textarea"

byId :: String -> Selector
byId = wrap <<< ("#" <> _)

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

reactSetValueP :: Selector -> String -> Toki.Page -> Aff Unit
reactSetValueP selector value page = void
  $ flip Toki.unsafeEvaluateStringFunction page
  $ fold
      [ -- https://stackoverflow.com/questions/23892547/what-is-the-best-way-to-trigger-onchange-event-in-react-js
        -- Nami uses react, which complicates things a bit
        "var input = $('" <> unwrap selector <> "').get(0);"
      , "var nativeInputValueSetter = "
          <>
            " Object.getOwnPropertyDescriptor(window.HTMLPasswordElement.prototype, 'value').set;"
      , "nativeInputValueSetter.call(input, '" <> value <> "');"
      , "var ev2 = new Event('input', { bubbles: true});"
      , "input.dispatchEvent(ev2);"
      ]

foreign import _retrieveJQuery :: Toki.Page -> Effect (Promise String)
foreign import _typeInto
  :: Selector -> String -> Toki.Page -> Effect (Promise Unit)

foreign import _clickTab :: Toki.Browser -> Toki.Page -> Effect Unit
foreign import _openPopup :: Toki.Browser -> Effect (Promise Unit)

clickTab :: Toki.Browser -> Toki.Page -> Aff Unit
clickTab b p = liftEffect $ _clickTab b p

openPopup :: Toki.Browser -> Aff Unit
openPopup b = toAffE $ _openPopup b
