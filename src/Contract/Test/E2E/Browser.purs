module Contract.Test.E2E.Browser
  ( Mode(Headless, Visible)
  , TestOptions(TestOptions)
  , withBrowser
  , parseOptions
  ) where

import Prelude

import Contract.Test.E2E.Helpers
  ( WalletPassword(WalletPassword)
  )
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
  , str
  , strOption
  , switch
  , value
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
  , wallets :: Map WalletExt WalletConfig
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
  eternlDir <- option (Just <$> str) $ fold
    [ long "eternl-dir"
    , metavar "DIR"
    , help "Directory where Eternl is unpacked"
    , value Nothing
    ]
  eternlPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "eternl-password"
    , metavar "PW"
    , help "Eternl wallet password"
    , value Nothing
    ]
  namiDir <- option (Just <$> str) $ fold
    [ long "nami-dir"
    , metavar "DIR"
    , help "Directory where nami is unpacked"
    , value Nothing
    ]
  namiPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "nami-password"
    , metavar "PW"
    , help "Nami wallet password"
    , value Nothing
    ]
  geroDir <- option (Just <$> str) $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  geroPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "gero-password"
    , metavar "PW"
    , help "Gero wallet password"
    , value Nothing
    ]
  flintDir <- option (Just <$> str) $ fold
    [ long "flint-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  flintPassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "flint-password"
    , metavar "PW"
    , help "Flint wallet password"
    , value Nothing
    ]
  lodeDir <- option (Just <$> str) $ fold
    [ long "lode-dir"
    , metavar "DIR"
    , help "Directory where lode is unpacked"
    , value Nothing
    ]
  lodePassword <- option (Just <<< WalletPassword <$> str) $ fold
    [ long "lode-password"
    , metavar "PW"
    , help "Lode wallet password"
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
  let
    wallets = Map.fromFoldable $ catMaybes
      [ mkConfig NamiExt namiDir namiPassword
      , mkConfig GeroExt geroDir geroPassword
      , mkConfig FlintExt flintDir flintPassword
      , mkConfig LodeExt lodeDir lodePassword
      , mkConfig EternlExt eternlDir eternlPassword
      ]
  in
    TestOptions
      { chromeExe, wallets, chromeUserDataDir, noHeadless }
  where
  mkConfig
    :: WalletExt
    -> Maybe FilePath
    -> Maybe WalletPassword
    -> Maybe (Tuple WalletExt WalletConfig)
  mkConfig ext mfp mpw = map (ext /\ _) $ WalletConfig <$> mfp <*> mpw

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
  Just (WalletConfig path _) -> pure <$> Toppokki.launch
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
