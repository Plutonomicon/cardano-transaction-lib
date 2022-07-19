module Contract.Test.Helpers
  ( hasSelector
  , startExample
  , runE2ETest
  , withExample
  , exampleUrl
  , doJQ
  , click
  , checkSuccess
  , setAttr
  , buttonWithText
  , password
  , jqStr
  , jqSet
  , setText
  , setValue
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
  , Selector(..)
  , Action(..)
  , RunningExample(..)
  , E2EOutput
  , delaySec
  ) where

import Prelude

import Contract.Test.Browser (TestOptions, withBrowser, WalletExt)
import Contract.Test.Feedback (resetTestFeedback, testFeedbackIsTrue)
import Control.Monad.Error.Class (try)
import Control.Promise (Promise, toAffE)
import Data.Array (head, filterA)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Traversable (for, fold)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1, EffectFn1)
import Foreign (Foreign, unsafeFromForeign)
import Mote (test)
import Test.Spec.Assertions (shouldSatisfy)
import TestM (TestPlanM)
import Toppokki as Toppokki

exampleUrl :: Toppokki.URL
exampleUrl = wrap "http://localhost:4008/"

testPassword :: String
testPassword = "ctlctlctl"

testPasswordGero :: String
testPasswordGero = "VZVfu5rp1r"

data OutputType = PageError | Console | RequestFailed

derive instance Eq OutputType

newtype E2EOutput = E2EOutput
  { outputType :: OutputType
  , output :: String
  }

newtype RunningExample = RunningExample
  { browser :: Toppokki.Browser
  , jQuery :: String
  , main :: Toppokki.Page
  , errors :: Ref (Array E2EOutput)
  }

derive instance Newtype RunningExample _

-- | Download jQuery
retrieveJQuery :: Toppokki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

-- | Simulate physical typing into a form element.
typeInto :: Selector -> String -> Toppokki.Page -> Aff Unit
typeInto selector text page = toAffE $ _typeInto selector text page

-- | Count how many items match a selector
jQueryCount :: Selector -> Toppokki.Page -> Aff Int
jQueryCount selector page = unsafeFromForeign <$> doJQ selector (wrap "length")
  page

-- | Check if a selector is matched on a page
hasSelector :: Selector -> Toppokki.Page -> Aff Boolean
hasSelector selector page = (_ > 0) <$> jQueryCount selector page

-- | Find the popup page of the wallet. This works for both Nami and Gero
-- | by looking for a page with a button. If there is a button on the main page
-- | this needs to be modified.
findWalletPage :: String -> Toppokki.Browser -> Aff (Maybe Toppokki.Page)
findWalletPage jQuery browser = do
  pages <- injectJQueryAll jQuery browser
  pages' <- filterA (hasSelector button) pages
  pure $ head $ pages'

-- | Wait until the wallet page pops up. Timout should be at least a few seconds.
-- | The 'String' param is the text of jQuery, which will be injected.
waitForWalletPage :: String -> Number -> Toppokki.Browser -> Aff Toppokki.Page
waitForWalletPage jQuery timeout browser =
  findWalletPage jQuery browser >>= case _ of
    Nothing -> do
      if timeout > 0.0 then do
        delaySec 0.1
        waitForWalletPage jQuery (timeout - 0.1) browser
      else liftEffect $ throw "Wallet did not open"
    Just page -> pure page

showOutput :: Ref (Array E2EOutput) -> Effect String
showOutput ref =
  Ref.read ref >>= map show' >>> intercalate "\n" >>> pure
  where
  show' :: E2EOutput -> String
  show' (E2EOutput { outputType, output }) = showType outputType <> " " <>
    output

  showType :: OutputType -> String
  showType PageError = "ERR"
  showType Console = "..."
  showType RequestFailed = "REQ"

-- | Navigate to an example's page, inject jQuery and set up error handlers
startExample :: String -> Toppokki.Browser -> Aff RunningExample
startExample name browser = do
  page <- Toppokki.newPage browser
  jQuery <- retrieveJQuery page
  errorRef <- liftEffect $ Ref.new []
  liftEffect do
    Toppokki.onPageError (handler errorRef PageError $ pure <<< show) page
    Toppokki.onConsole (handler errorRef Console Toppokki.consoleMessageText)
      page
    Toppokki.onRequestFailed (handler errorRef RequestFailed $ pure <<< show)
      page
  Toppokki.goto url page
  pure $ wrap
    { browser: browser
    , jQuery: jQuery
    , main: page
    , errors: errorRef
    }
  where
  url :: Toppokki.URL
  url = wrap $ unwrap exampleUrl <> "?" <> name

  -- Setup a handler for an output type.
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

-- | Run an example in the browser and close the browser afterwards
withExample
  :: forall (a :: Type)
   . String
  -> Toppokki.Browser
  -> (RunningExample -> Aff a)
  -> Aff a
withExample example browser = bracket (startExample example browser)
  (const $ pure unit)

runE2ETest
  :: forall (a :: Type)
   . String
  -> TestOptions
  -> WalletExt
  -> (RunningExample -> Aff a)
  -> TestPlanM Unit
runE2ETest example opts ext f = test example $ withBrowser opts ext $
  \browser -> withExample example browser
    ( \e -> do
        liftEffect $ log $ "Start Example " <> example
        resetTestFeedback (_.main $ unwrap e)
        void $ try $ f e
        delaySec 10.0
        liftEffect $ log $ "Example " <> example <>
          " finished, check success..."
        checkSuccess e
    )

checkSuccess :: RunningExample -> Aff Unit
checkSuccess (RunningExample { main, errors }) = do
  feedback <- testFeedbackIsTrue main
  unless feedback $ liftEffect $ showOutput errors >>= log
  shouldSatisfy feedback (_ == true)

inWalletPage
  :: forall (a :: Type). RunningExample -> (Toppokki.Page -> Aff a) -> Aff a
inWalletPage (RunningExample { browser, jQuery }) =
  (waitForWalletPage jQuery 10.0 browser >>= _)

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
    delaySec 0.1
    void $ doJQ (inputType "radio") click page
    void $ doJQ (buttonWithText "Continue") click page
    delaySec 0.1
    void $ doJQ (inputType "checkbox") click page
    void $ doJQ (buttonWithText "Connect") click page

geroSign :: RunningExample -> Aff Unit
geroSign =
  flip inWalletPage $ \gero -> do
    void $ doJQ (byId "confirm-swap") click gero
    typeInto (byId "wallet-password") testPasswordGero gero
    clickButton "Next" gero

-- | A String representing a jQuery selector, e.g. "#my-id" or ".my-class"
newtype Selector = Selector String

derive instance Newtype Selector _

-- | A String representing a jQuery action, e.g. "click" or "enable".
newtype Action = Action String

derive instance Newtype Action _

-- | Build a primitive jQuery expression like '$("button").click()' and
-- | out of a selector and action and evaluate it in Toppokki
doJQ :: Selector -> Action -> Toppokki.Page -> Aff Foreign
doJQ selector action page = do
  Toppokki.unsafeEvaluateStringFunction jq page
  where
  jq :: String
  jq = "$('" <> unwrap selector <> "')." <> unwrap action

-- | set an attribute to a value
setAttr :: String -> String -> Action
setAttr attr value = wrap $ "attr(" <> jqStr attr <> ", " <> value <> ")"

-- | select any button
button :: Selector
button = wrap "button"

-- | select a button with a specific text inside
buttonWithText :: String -> Selector
buttonWithText text = wrap $ "button:contains(" <> text <> ")"

-- | select an input element of a specific type
inputType :: String -> Selector
inputType typ = wrap $ "input[type=\"" <> typ <> "\"]"

-- | select an element by Id
byId :: String -> Selector
byId = wrap <<< ("#" <> _)

-- | select any password field
password :: Selector
password = wrap ":password"

-- | wrap a string inside quotation marks (intended: make a jQuery string).
jqStr :: String -> String
jqStr str = "\"" <> str <> "\""

-- | set something to value with jQuery, i.e. call $(...).<thing>(<value>)
jqSet :: String -> String -> Action
jqSet what value = wrap $ what <> "(" <> value <> ")"

-- | set the text
setText :: String -> Action
setText = jqSet "text" <<< jqStr

-- | set the value
setValue :: String -> Action
setValue = jqSet "val" <<< jqStr

click :: Action
click = wrap "click()"

clickButton :: String -> Toppokki.Page -> Aff Unit
clickButton buttonText = void <$> doJQ (buttonWithText buttonText) click

-- | inject jQuery into all pages in the browser which don't have it yet.
injectJQueryAll :: String -> Toppokki.Browser -> Aff (Array Toppokki.Page)
injectJQueryAll jQuery browser = do
  pages <- Toppokki.pages browser
  void $ for pages $ \page -> do
    (alreadyInjected :: Boolean) <-
      unsafeFromForeign <$>
        Toppokki.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
          page
    unless alreadyInjected $ void $ Toppokki.unsafeEvaluateStringFunction jQuery
      page
  pure pages

-- | Set the value of an item with the browser's native value setter.
-- | This is necessary for react items so that react reacts.
-- | (sometimes 'typeInto' is an alternative).
-- | React is used in Nami.
-- | https://stackoverflow.com/questions/23892547/what-is-the-best-way-to-trigger-onchange-event-in-react-js
reactSetValue :: Selector -> String -> Toppokki.Page -> Aff Unit
reactSetValue selector value page = void
  $ flip Toppokki.unsafeEvaluateStringFunction page
  $ fold
      [ "let input = $('" <> unwrap selector <> "').get(0);"
      , "var nativeInputValueSetter = "
          <>
            " Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, 'value').set;"
      , "nativeInputValueSetter.call(input, '" <> value <> "');"
      , "var ev2 = new Event('input', { bubbles: true});"
      , "input.dispatchEvent(ev2);"
      ]

foreign import _retrieveJQuery :: Toppokki.Page -> Effect (Promise String)

foreign import _typeInto
  :: Selector -> String -> Toppokki.Page -> Effect (Promise Unit)

delaySec :: Number -> Aff Unit
delaySec seconds = delay $ wrap $ seconds * 1000.0

{-
-- This can initialize gero and import a walllet without storing any config data
-- anywhere. However, there doesn't seem to be a straightforward way to set the collateral,
-- which makes it rather useless, as this would involve opening the extension popup window,
-- and that is currently not supported in Puppeteer.

geroInit :: Toppokki.Browser -> Aff Unit
geroInit browser = do
  page <- Toppokki.newPage browser
  jQuery <- retrieveJQuery page
  Toppokki.goto
    (wrap "chrome-extension://iifeegfcfhlhhnilhfoeihllenamcfgc/index.html")
    page
  void $ Toppokki.unsafeEvaluateStringFunction jQuery page
  delay $ wrap 1000.0
  void $ doJQ (buttonWithText "Get Started") click page
  delay $ wrap 1000.0
  void $ doJQ (wrap "a[href=\"#/wallet-options/import-wallet\"]") click page
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
-}
