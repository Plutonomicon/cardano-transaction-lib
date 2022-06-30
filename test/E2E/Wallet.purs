module Test.E2E.Wallet where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (typeOf)
import Mote (group, test)
import TestM (TestPlanM)
import Toppokki as Toki
import Test.Examples.Config (userDataDir)
import Test.Spec.Assertions (shouldEqual)

namiHash :: String
namiHash = "lpfcbjknijpeeillifnkikgncikgfhdo"

namiPath :: String
namiPath = "/home/mike/.config/google-chrome/Default/Extensions/" <> namiHash <>
  "/3.2.5_1"

data Mode = Headless | Visible

derive instance Eq Mode

chromeArgsWithNami :: Mode -> Array String
chromeArgsWithNami mode =
  [ "--disable-extensions-except=" <> namiPath
  , "--load-extension=" <> namiPath
  ] <> if mode == Headless then [ "--headless=chrome" ] else []

launchWithNami :: Mode -> Aff Toki.Browser
launchWithNami mode =
  Toki.launch
    { args: chromeArgsWithNami mode
    , headless: mode == Headless
    , userDataDir: userDataDir
    }

launchWithNami' :: Aff Toki.Browser
launchWithNami' = launchWithNami Headless

-- | To access the wallet without navigating to a page
initializeNami :: Toki.Browser -> Aff Toki.Page
initializeNami browser = do
  page <- Toki.newPage browser
  outputJs <- liftEffect _outputJsPath
  _ <- flip Toki.unsafeEvaluateStringFunction page $
    "document.write(\"<html><head>"
      <> "<script src='"
      <> jsPage
      <> "' type='text/javascript'></script>"
      <> "<script src='file:///'"
      <> outputJs
      <> " type='text/javascript'></script>"
      <> "</head><body></body></html>\");"
  --  _ <- pageWaitForSelector (Selector "window") {} page
  pure page
  where
  jsPage =
    "chrome-extension://lpfcbjknijpeeillifnkikgncikgfhdo/injected.bundle.js"

suite :: TestPlanM Unit
suite = group "Nami" $ do

  test "Launch headless Chrome with Nami" $ do
    _ <- launchWithNami'
    pure unit

  test "Initialize Nami" $ do
    browser <- launchWithNami'
    page <- initializeNami browser
    fgn <- Toki.unsafeEvaluateStringFunction "window.cardano" page
    typeOf fgn `shouldEqual` "object"

  test "getNamiWalletAddress" $ do
    browser <- launchWithNami'
    page <- initializeNami browser
    fgn <- Toki.unsafeEvaluateStringFunction "window.cardano" page
    typeOf fgn `shouldEqual` "object"

  test "getAlwaysSucceedsExample" $ do
    browser <- launchWithNami'
    page <- initializeNami browser
    fgn <- Toki.unsafeEvaluateStringFunction "PS['Examples.AlwaysSucceeds']"
      page
    typeOf fgn `shouldEqual` "object"

--    wallet <- mkNamiWalletAff
--    pure unit
{-    case wallet of
      Nami wallet' -> do
        address <- wallet'.getWalletAddress wallet'.connection
        address `shouldEqual` Nothing        
      Gero _ -> true `shouldEqual` false
-}

foreign import _outputJsPath :: Effect String

{-
foreign import _confirmNamiAccess :: Effect Unit

confirmNamiAccess :: Page -> Aff Unit
confirmNamiAccess = void <$> Toki.unsafeEvaluateStringFunction "_confirmNamiAccess"
-}

