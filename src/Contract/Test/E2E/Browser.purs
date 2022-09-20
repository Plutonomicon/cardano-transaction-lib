module Contract.Test.E2E.Browser
  ( withBrowser
  , module X
  ) where

import Prelude

import Contract.Test.E2E.Helpers (WalletPassword(WalletPassword))
import Contract.Test.E2E.WalletExt
  ( WalletConfig(WalletConfig)
  , WalletExt(LodeExt, FlintExt, GeroExt, NamiExt, EternlExt)
  )
import Data.Array (catMaybes)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
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
import Contract.Test.E2E.Options
import Contract.Test.E2E.Options
  ( Mode(Headless, Visible)
  , TestOptions
  , parseOptions
  ) as X

withBrowser
  :: forall (a :: Type)
   . TestOptions
  -> WalletExt
  -> (Maybe Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension opts ext)
  ( \mbrowser -> case mbrowser of
      Nothing -> pure unit
      Just b -> Toppokki.close b
  )

launchWithExtension
  :: TestOptions
  -> WalletExt
  -> Aff (Maybe Toppokki.Browser)
launchWithExtension
  { chromeExe, chromeUserDataDir, noHeadless, wallets }
  walletExt = case Map.lookup walletExt wallets of
  Nothing -> pure Nothing
  Just (WalletConfig path _) -> pure <$> Toppokki.launch
    { args:
        [ "--disable-extensions-except=" <> path
        , "--load-extension=" <> path
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
