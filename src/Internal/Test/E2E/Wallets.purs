-- | Contains functions to sign/confirm access for supported wallets.
module Ctl.Internal.Test.E2E.Wallets
  ( eternlConfirmAccess
  , eternlSign
  , geroConfirmAccess
  , geroSign
  , flintConfirmAccess
  , flintSign
  , lodeConfirmAccess
  , lodeSign
  , namiConfirmAccess
  , namiSign
  , laceConfirmAccess
  , laceSign
  ) where

import Prelude

import Control.MonadPlus (guard)
import Control.Promise (Promise, toAffE)
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Test.E2E.Types
  ( ExtensionId
  , RunningE2ETest
  , WalletPassword
  , unExtensionId
  )
import Data.Array as Array
import Data.Either (fromRight)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Foreign (Foreign, unsafeFromForeign)
import Toppokki as Toppokki

-- | Simulate physical typing into a form element.
typeInto :: Selector -> String -> Toppokki.Page -> Aff Unit
typeInto selector text page = toAffE $ _typeInto selector text page

-- | Find the popup page of the wallet.
-- | Accepts a URL pattern that identifies the extension popup (e.g. extension
-- | ID). Throws if there is more than one page matching the pattern.
findWalletPage :: Pattern -> Toppokki.Browser -> Aff (Maybe Toppokki.Page)
findWalletPage pattern browser = do
  pages <- Toppokki.pages browser
  walletPages <- Array.catMaybes <$> for pages \page -> do
    url <- liftEffect $ pageUrl page
    pure do
      guard (String.contains pattern url)
      pure page
  case walletPages of
    [] -> pure Nothing
    [ page ] -> pure $ Just page
    foundPages -> do
      urls <- for foundPages (liftEffect <<< pageUrl)
      liftEffect $ throw $
        "findWalletPage: more than one page found when trying to find "
          <> "the wallet popup. URLs: "
          <> intercalate ", " urls
          <> "; URL pattern: "
          <> show (unwrap pattern)

foreign import pageUrl :: Toppokki.Page -> Effect String

-- | Wait until the wallet page pops up. Timout should be at least a few seconds.
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
  -> RunningE2ETest
  -> Seconds
  -> (Toppokki.Page -> Aff a)
  -> Aff a
inWalletPage pattern { browser, jQuery } timeout cont = do
  page <- waitForWalletPage pattern timeout browser
  for_ jQuery $ injectJQuery page
  cont page

-- | Provide an extension ID and a pattern to match.
-- | First, we wait for a popup with given extension ID.
-- | If the URL matches the pattern, the action is executed.
-- | Otherwise, `Nothing` is returned.
inWalletPageOptional
  :: forall (a :: Type)
   . ExtensionId
  -> Pattern
  -> RunningE2ETest
  -> Seconds
  -> (Toppokki.Page -> Aff a)
  -> Aff (Maybe a)
inWalletPageOptional extId pattern { browser, jQuery } timeout cont = do
  try acquirePage <#> fromRight Nothing
  where
  acquirePage = do
    page <- waitForWalletPage (Pattern $ unExtensionId extId) timeout browser
    url <- liftEffect $ pageUrl page
    if String.contains pattern url then do
      for_ jQuery $ injectJQuery page
      Just <$> cont page
    else do
      pure Nothing

eternlConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
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

eternlSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
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

namiConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
namiConfirmAccess extId re = do
  wasInPage <- isJust <$> inWalletPageOptional extId pattern re
    confirmAccessTimeout
    (clickButton "Access")
  when wasInPage do
    waitForWalletPageClose pattern 10.0 re.browser
  where
  pattern :: Pattern
  pattern = wrap $ unExtensionId extId

namiSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
namiSign extId wpassword re = do
  inWalletPage (Pattern $ unExtensionId extId) re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap ".chakra-button") {} page
    clickButton "Sign" page
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "password")
      {}
      page
    typeInto (inputType "password") wpassword page
    clickButton "Confirm" page

geroConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
geroConfirmAccess extId re = do
  wasInPage <- isJust <$> inWalletPageOptional extId pattern re
    confirmAccessTimeout
    \page -> do
      void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "radio")
        {}
        page
      void $ doJQ (inputType "radio") click page
      void $ doJQ (buttonWithText "Continue") click page
      void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "checkbox")
        {}
        page
      void $ doJQ (inputType "checkbox") click page
      void $ doJQ (buttonWithText "Connect") click page
  when wasInPage do
    waitForWalletPageClose pattern 10.0 re.browser
  where
  pattern :: Pattern
  pattern = wrap $ unExtensionId extId <> "/index.html?#/connection"

geroSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
geroSign extId password re =
  inWalletPage pattern re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ byId "confirm-swap")
      {}
      page
    void $ doJQ (byId "confirm-swap") click page
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ byId "wallet-password")
      {}
      page
    typeInto (byId "wallet-password") password page
    clickButton "Next" page
  where
  pattern = Pattern $ unExtensionId extId <> "/index.html?#/swap?tx="

laceConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
laceConfirmAccess extId re = do
  void $ liftEffect $ throw "Lace support is not implemented"
  wasInPage <- isJust <$> inWalletPageOptional extId pattern re
    confirmAccessTimeout
    \page -> do
      void $ Toppokki.pageWaitForSelector (wrap $ "button")
        {}
        page
      delaySec 1.0
      void $ doJQ (buttonWithText "Authorize") click page
      delaySec 0.1
      void $ doJQ (buttonWithText "Always") click page
  when wasInPage do
    waitForWalletPageClose pattern 10.0 re.browser
  -- Trigger wallet page opening.
  -- Lace has a glitch that prevents CIP-30 API promises from resolving until
  -- the wallet is interacted with at least once after browser startup.
  -- https://discord.com/channels/826816523368005654/1050085066056925195/1091336339250749511
  -- Toppokki.newPage re.browser >>=
  --   Toppokki.goto (wrap $ "chrome-extension://" <> unExtensionId extId <> "/app.html#/assets")
  where
  pattern :: Pattern
  pattern = wrap $ unExtensionId extId <> "/dappConnector.html"

-- Not implemented yet
laceSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
laceSign extId password re = do
  void $ liftEffect $ throw "Lace support is not implemented"
  inWalletPage pattern re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap $ "button")
      {}
      page
    delaySec 0.1
    clickButton "Confirm" page
    delaySec 0.1
    typeInto (inputType "password") password page
    delaySec 0.1
    clickButton "Confirm" page
    delaySec 1.0
    clickButton "Close" page
  -- TODO: continue from here
  where
  pattern = Pattern $ unExtensionId extId <> "/dappConnector.html#/dapp/sign-tx"

-- Not implemented yet
flintConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
flintConfirmAccess _ _ =
  liftEffect $ throw "Flint support is not implemented"

-- Not implemented yet
flintSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
flintSign _ _ _ = do
  liftEffect $ throw "Flint support is not implemented"

getJQuery :: RunningE2ETest -> Aff String
getJQuery re =
  liftM (error errMessage) re.jQuery
  where
  errMessage =
    "JQuery not available (wrong E2E runtime type? Please report as bug)"

lodeConfirmAccess :: ExtensionId -> RunningE2ETest -> Aff Unit
lodeConfirmAccess extId re = do
  wasOnAccessPage <- inWalletPage pattern re confirmAccessTimeout \page -> do
    delaySec 0.1
    isOnAccessPage <- getJQuery re >>= isJQuerySelectorAvailable
      (buttonWithText "Access")
      page

    when isOnAccessPage do
      void $ doJQ (buttonWithText "Access") click page
    pure isOnAccessPage
  when wasOnAccessPage do
    waitForWalletPageClose pattern 10.0 re.browser
  where
  pattern = Pattern $ unExtensionId extId

lodeSign :: ExtensionId -> WalletPassword -> RunningE2ETest -> Aff Unit
lodeSign extId gpassword re = do
  inWalletPage (Pattern $ unExtensionId extId) re signTimeout \page -> do
    void $ Toppokki.pageWaitForSelector (wrap $ unwrap $ inputType "password")
      {}
      page
    isOnSignPage <- getJQuery re >>= isJQuerySelectorAvailable
      (buttonWithText "Approve")
      page
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

-- | Inject jQuery into a running page
injectJQuery :: Toppokki.Page -> String -> Aff Unit
injectJQuery page jQuery = do
  (alreadyInjected :: Boolean) <- unsafeFromForeign <$>
    Toppokki.unsafeEvaluateStringFunction "typeof(jQuery) !== 'undefined'"
      page
  unless alreadyInjected $ void $ Toppokki.unsafeEvaluateStringFunction jQuery
    page

foreign import _typeInto
  :: Selector -> String -> Toppokki.Page -> Effect (Promise Unit)
