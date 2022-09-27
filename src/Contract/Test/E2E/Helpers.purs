-- | Contains functions to sign/confirm access for supported wallets.
module Contract.Test.E2E.Helpers
  ( delaySec
  , eternlConfirmAccess
  , eternlSign
  , geroConfirmAccess
  , geroSign
  , flintConfirmAccess
  , flintSign
  , lodeConfirmAccess
  , lodeSign
  , namiConfirmAccess
  , namiSign
  , withExample
  , module X
  ) where

import Prelude

import Contract.Test.E2E.Types
  ( ExtensionId
  , RunningExample
  , WalletPassword
  , unExtensionId
  )
import Contract.Test.E2E.Types (ExtensionId, WalletPassword) as X
import Control.Promise (Promise, toAffE)
import Data.Array (head)
import Data.Either (fromRight, hush)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (for, fold)
import Effect (Effect)
import Effect.Aff (Aff, bracket, delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Foreign (Foreign, unsafeFromForeign)
import Helpers (liftedM)
import Toppokki as Toppokki

-- | Run an example in the browser and close the browser afterwards.
-- |
-- | Sample usage:
-- | ```purescript
-- |   withBrowser options NamiExt \browser -> do
-- |     withExample
-- |        (wrap "http://myserver:1234/docontract")
-- |        browser do
-- |          namiSign $ wrap "mypassword"
-- | ```
withExample
  :: forall (a :: Type)
   . Toppokki.URL
  -> Toppokki.Browser
  -> (RunningExample -> Aff a)
  -> Aff a
withExample url browser = bracket (startExample url browser)
  (const $ pure unit)

-- | Navigate to an example's page, inject jQuery and set up error handlers
startExample :: Toppokki.URL -> Toppokki.Browser -> Aff RunningExample
startExample url browser = do
  page <- Toppokki.newPage browser
  jQuery <- retrieveJQuery page
  -- TODO: pipe logs
  -- liftEffect do
  --   Toppokki.onPageError (handler errors PageError $ pure <<< show) page
  --   Toppokki.onConsole (handler errors Console Toppokki.consoleMessageText)
  --     page
  --   Toppokki.onRequestFailed (handler errors RequestFailed $ pure <<< show)
  --     page
  Toppokki.goto url page
  pure
    { browser
    , jQuery
    , page
    }

-- | Simulate physical typing into a form element.
typeInto :: Selector -> String -> Toppokki.Page -> Aff Unit
typeInto selector text page = toAffE $ _typeInto selector text page

-- | Find the popup page of the wallet. This works for both Nami and Gero
-- | by looking for a page with a button. If there is a button on the main page
-- | this needs to be modified.
findWalletPage :: Pattern -> Toppokki.Browser -> Aff (Maybe Toppokki.Page)
findWalletPage pattern browser = do
  pages <- Toppokki.pages browser
  head <<< fold <$> for pages \page -> do
    eiPageUrl <- map hush $ try $ pageUrl page
    pure $
      case eiPageUrl of
        Nothing -> []
        Just url
          | String.contains pattern url -> [ page ]
          | otherwise -> []

pageUrl :: Toppokki.Page -> Aff String
pageUrl page = do
  unsafeFromForeign <$> Toppokki.unsafeEvaluateStringFunction
    "document.location.href"
    page

-- | Wait until the wallet page pops up. Timout should be at least a few seconds.
-- | The 'String' param is the text of jQuery, which will be injected.
waitForWalletPage
  :: Pattern -> Seconds -> Toppokki.Browser -> Aff Toppokki.Page
waitForWalletPage pattern timeout browser =
  findWalletPage pattern browser >>= case _ of
    Nothing -> do
      if timeout > Seconds 0.0 then do
        delaySec 0.1
        waitForWalletPage pattern (Seconds $ unwrap timeout - 0.1) browser
      else liftEffect $ throw $
        "Wallet popup did not open. Did you provide extension ID correctly? "
          <> "Provided pattern: "
          <> unwrap pattern
    Just page -> pure page

waitForWalletPageClose
  :: Pattern -> Number -> Toppokki.Browser -> Aff Unit
waitForWalletPageClose pattern timeout browser =
  findWalletPage pattern browser >>= case _ of
    Nothing -> do
      pure unit
    Just _page -> do
      if timeout > 0.0 then do
        delaySec 0.1
        waitForWalletPageClose pattern (timeout - 0.1) browser
      else liftEffect $ throw $
        "Wallet popup did not close. Did you provide extension ID correctly? "
          <> "Provided pattern: "
          <> unwrap pattern

-- | Find the wallet extension popup given a URL pattern
inWalletPage
  :: forall (a :: Type)
   . Pattern
  -> RunningExample
  -> Seconds
  -> (Toppokki.Page -> Aff a)
  -> Aff a
inWalletPage pattern { browser, jQuery } timeout cont = do
  page <- waitForWalletPage pattern timeout browser
  injectJQuery page jQuery
  cont page

-- | Provide an extension ID and a pattern to match.
-- | First, we wait for a popup with given extension ID.
-- | If the URL matches the pattern, the action is executed.
-- | Otherwise, `Nothing` is returned.
inWalletPageOptional
  :: forall (a :: Type)
   . ExtensionId
  -> Pattern
  -> RunningExample
  -> Seconds
  -> (Toppokki.Page -> Aff a)
  -> Aff (Maybe a)
inWalletPageOptional extId pattern { browser, jQuery } timeout cont = do
  try acquirePage <#> fromRight Nothing
  where
  acquirePage = do
    page <- waitForWalletPage (Pattern $ unExtensionId extId) timeout browser
    url <- liftedM (error "inWalletPageOptional: page closed")
      $ map hush
      $ try
      $ pageUrl page
    if
      String.contains pattern url then do
      injectJQuery page jQuery
      Just <$> cont page
    else do
      pure Nothing

eternlConfirmAccess :: ExtensionId -> RunningExample -> Aff Unit
eternlConfirmAccess extId re = do
  wasInPage <- isJust <$> inWalletPageOptional extId pattern re
    confirmAccessTimeout
    \page -> do
      delaySec 1.0
      void $ Toppokki.pageWaitForSelector (wrap "span.capitalize") {} page
      clickButton "Connect to Site" page
  when wasInPage do
    waitForWalletPageClose pattern 10.0 re.browser
  where
  pattern :: Pattern
  pattern = wrap $ unExtensionId extId <> "/www/index.html#/connect/"

eternlSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
eternlSign extId password re = do
  inWalletPage pattern re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "password")
      {}
      page
    -- TODO: why does it require a delay?
    delaySec 1.0
    typeInto (inputType "password") password page
    void $ doJQ (wrap "span.capitalize:contains(\"sign\")") click page
  where
  pattern :: Pattern
  pattern = wrap $ unExtensionId extId <> "/www/index.html#/signtx"

namiConfirmAccess :: ExtensionId -> RunningExample -> Aff Unit
namiConfirmAccess extId re =
  inWalletPage (Pattern $ unExtensionId extId) re confirmAccessTimeout
    (clickButton "Access")

namiSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
namiSign extId wpassword re = do
  inWalletPage (Pattern $ unExtensionId extId) re signTimeout \nami -> do
    clickButton "Sign" nami
    reactSetValue (Selector ":password") wpassword nami
    clickButton "Confirm" nami

geroConfirmAccess :: ExtensionId -> RunningExample -> Aff Unit
geroConfirmAccess extId re = do
  inWalletPage (Pattern $ unExtensionId extId) re confirmAccessTimeout \page ->
    do
      delaySec 0.1
      void $ doJQ (inputType "radio") click page
      void $ doJQ (buttonWithText "Continue") click page
      delaySec 0.1
      void $ doJQ (inputType "checkbox") click page
      void $ doJQ (buttonWithText "Connect") click page

geroSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
geroSign extId password re =
  inWalletPage (Pattern $ unExtensionId extId) re signTimeout \gero -> do
    void $ doJQ (byId "confirm-swap") click gero
    typeInto (byId "wallet-password") password gero
    clickButton "Next" gero

-- Not implemented yet
flintConfirmAccess :: ExtensionId -> RunningExample -> Aff Unit
flintConfirmAccess _ _ =
  liftEffect $ throw "Flint support is not implemented"

-- Not implemented yet
flintSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
flintSign _ _ _ = do
  liftEffect $ throw "Flint support is not implemented"

lodeConfirmAccess :: ExtensionId -> RunningExample -> Aff Unit
lodeConfirmAccess extId re = do
  wasOnAccessPage <- inWalletPage pattern re confirmAccessTimeout \page -> do
    delaySec 0.1
    isOnAccessPage <- isJQuerySelectorAvailable (buttonWithText "Access") page
      re.jQuery
    when isOnAccessPage do
      void $ doJQ (buttonWithText "Access") click page
    pure isOnAccessPage
  when wasOnAccessPage do
    waitForWalletPageClose pattern 10.0 re.browser
  where
  pattern = Pattern $ unExtensionId extId

lodeSign :: ExtensionId -> WalletPassword -> RunningExample -> Aff Unit
lodeSign extId gpassword re = do
  inWalletPage (Pattern $ unExtensionId extId) re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "password")
      {}
      page
    isOnSignPage <- isJQuerySelectorAvailable (buttonWithText "Approve") page
      re.jQuery
    unless isOnSignPage do
      liftEffect $ throw $ "lodeSign: unable to find signing page"
    typeInto (inputType "password") gpassword page
    delaySec 0.1
    clickButton "Approve" page

isJQuerySelectorAvailable :: Selector -> Toppokki.Page -> String -> Aff Boolean
isJQuerySelectorAvailable selector page jQuery = do
  injectJQuery page jQuery
  unsafeFromForeign <$> Toppokki.unsafeEvaluateStringFunction
    ("!!$('" <> unwrap selector <> "').length")
    page

confirmAccessTimeout :: Seconds
confirmAccessTimeout = wrap 10.0

signTimeout :: Seconds
signTimeout = wrap 50.0

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

-- | select a button with a specific text inside
buttonWithText :: String -> Selector
buttonWithText text = wrap $ "button:contains(" <> text <> ")"

-- | select an input element of a specific type
inputType :: String -> Selector
inputType typ = wrap $ "input[type=\"" <> typ <> "\"]"

-- | select an element by Id
byId :: String -> Selector
byId = wrap <<< ("#" <> _)

click :: Action
click = wrap "click()"

clickButton :: String -> Toppokki.Page -> Aff Unit
clickButton buttonText = void <$> doJQ (buttonWithText buttonText) click

delaySec :: Number -> Aff Unit
delaySec seconds = delay $ wrap $ seconds * 1000.0

-- | Download jQuery
retrieveJQuery :: Toppokki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

-- | Inject jQuery into a running page
injectJQuery :: Toppokki.Page -> String -> Aff Unit
injectJQuery page jQuery = do
  (alreadyInjected :: Boolean) <- unsafeFromForeign <$>
    Toppokki.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
      page
  unless alreadyInjected $ void $ Toppokki.unsafeEvaluateStringFunction jQuery
    page

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
