module Contract.Test.E2E.Browser
  ( withBrowser
  , module X
  ) where

import Contract.Test.E2E.Options
import Prelude

import Contract.Test.E2E.Helpers (WalletPassword(WalletPassword))
import Contract.Test.E2E.Options
  ( Mode(Headless, Visible)
  , TestOptions
  , parseOptions
  ) as X
import Contract.Test.E2E.Runner (E2ETestRuntime)
import Contract.Test.E2E.WalletExt
  ( WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  )
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Node.Path (FilePath)
import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , help
  , info
  , long
  , metavar
  , option
  , showDefault
  , showDefaultWith
  , str
  , strOption
  , switch
  , value
  )
import Options.Applicative.Types (optional)
import Toppokki as Toppokki
import Undefined (undefined)

withBrowser
  :: forall (a :: Type)
   . Boolean
  -> E2ETestRuntime
  -> WalletExt
  -> (Maybe Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser noHeadless opts ext = bracket
  (launchWithExtension noHeadless opts ext)
  ( \mbrowser -> case mbrowser of
      Nothing -> pure unit
      Just b -> Toppokki.close b
  )

launchWithExtension
  :: Boolean
  -> E2ETestRuntime
  -> WalletExt
  -> Aff (Maybe Toppokki.Browser)
launchWithExtension
  noHeadless
  { browserPath, chromeUserDataDir, wallets }
  walletExt = case Map.lookup walletExt wallets of
  Nothing -> pure Nothing
  Just walletParams -> pure <$> Toppokki.launch
    { args:
        [ "--disable-extensions-except=" <> unwrap walletParams.extensionId
        , "--load-extension=" <> unwrap walletParams.extensionId
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: "" -- TODO: chromeUserDataDir
    , executablePath: "" -- TODO: fromMaybe "" chromeExe
    }
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless
