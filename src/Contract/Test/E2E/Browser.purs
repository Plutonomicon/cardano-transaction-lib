module Contract.Test.E2E.Browser
  ( Mode(Headless, Visible)
  , TestOptions(TestOptions)
  , withBrowser
  , parseOptions
  ) where

import Prelude

import Contract.Test.E2E.WalletExt
  ( SomeWallet
  , WalletExt(FlintExt, GeroExt, NamiExt)
  , getWalletByType
  )
import Data.Foldable (fold)
import Data.Array (catMaybes)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Path (FilePath)
import Options.Applicative
  ( Parser
  , execParser
  , help
  , long
  , metavar
  , option
  , showDefault
  , str
  , strOption
  , switch
  , value
  , info
  , fullDesc
  )
import Toppokki as Toppokki

-- | Parameters for E2E tests
-- | 'chromeExe' should point to a chromium or google-chrome binary.
-- | 'namiDir' should point to a directory where Nami is unpacked.
-- | 'geroDir' should point to a directory where Gero is unpacked.
-- |    both Nami and Gero must be provided to run the full suite of tests.
-- | 'chromeUserDataDir' should point to a chromium profile directory. If
-- |    not provided, test-data/chrome-user-data is used.
-- |    'make e2e-test' then takes care to unpack Nami and Gero settings to there,
-- |    so that wallet data is available.
-- | 'noHeadless' is a flag to display the browser during the tests.
data TestOptions = TestOptions
  { chromeExe :: Maybe FilePath
  , wallets :: Map WalletExt FilePath
  , chromeUserDataDir :: FilePath
  , noHeadless :: Boolean
  }

data Mode = Headless | Visible

derive instance Eq Mode

optParser :: Parser TestOptions
optParser = ado
  chromeExe <- option (Just <$> str) $ fold
    [ long "chrome-exe"
    , metavar "FILE"
    , help "Chrome/-ium exe (search in env if not set)"
    , value Nothing
    ]
  nami <- option ((\p -> Just $ NamiExt /\ p) <$> str) $ fold
    [ long "nami-dir"
    , metavar "DIR"
    , help "Directory where nami is unpacked"
    , value Nothing
    ]
  gero <- option ((\p -> Just $ GeroExt /\ p) <$> str) $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  flint <- option ((\p -> Just $ FlintExt /\ p) <$> str) $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  chromeUserDataDir <- strOption $ fold
    [ long "chrome-user-data"
    , help "Chrome/-ium user data dir"
    , value "test-data/chrome-user-data"
    , showDefault
    , metavar "DIR"
    ]
  noHeadless <- switch $ fold
    [ long "no-headless"
    , help "Show visible browser window"
    ]
  let wallets = Map.fromFoldable $ catMaybes [ nami, gero, flint ]
  in
    TestOptions
      { chromeExe, wallets, chromeUserDataDir, noHeadless }

parseOptions :: Effect TestOptions
parseOptions = execParser $ info optParser fullDesc

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
  ( TestOptions
      { chromeExe, chromeUserDataDir, noHeadless, wallets }
  )
  walletExt = case Map.lookup walletExt wallets of
  Nothing -> pure Nothing
  Just path -> pure <$> Toppokki.launch
    { args:
        [ "--disable-extensions-except=" <> path
        , "--load-extension=" <> path
        ] <> if mode == Headless then [ "--headless=chrome" ] else []
    , headless: mode == Headless
    , userDataDir: chromeUserDataDir
    , executablePath: fromMaybe "" chromeExe
    }
  where
  mode :: Mode
  mode
    | noHeadless = Visible
    | otherwise = Headless
