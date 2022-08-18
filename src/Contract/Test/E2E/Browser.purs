module Contract.Test.E2E.Browser
  ( Mode(Headless, Visible)
  , TestOptions(TestOptions)
  , withBrowser
  , parseOptions
  ) where

import Prelude

import Contract.Test.E2E.WalletExt
  ( class WalletExt
  , FlintExt(FlintExt)
  , GeroExt(GeroExt)
  , NamiExt(NamiExt)
  , SomeWallet
  , mkWallet
  , walletPath
  )
import Data.Foldable (fold)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
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
  , wallets :: Array SomeWallet
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
  nami <- option (Just <<< mkWallet <<< NamiExt <$> str) $ fold
    [ long "nami-dir"
    , metavar "DIR"
    , help "Directory where nami is unpacked"
    , value Nothing
    ]
  gero <- option (Just <<< mkWallet <<< GeroExt <$> str) $ fold
    [ long "gero-dir"
    , metavar "DIR"
    , help "Directory where gero is unpacked"
    , value Nothing
    ]
  flint <- option (Just <<< mkWallet <<< FlintExt <$> str) $ fold
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
  let wallets = catMaybes [ nami, gero, flint ]
  in
    TestOptions
      { chromeExe, wallets, chromeUserDataDir, noHeadless }

parseOptions :: Effect TestOptions
parseOptions = execParser $ info optParser fullDesc

withBrowser
  :: forall (a :: Type) (wallet :: Type)
   . WalletExt wallet
  => TestOptions
  -> wallet
  -> (Toppokki.Browser -> Aff a)
  -> Aff a
withBrowser opts ext = bracket (launchWithExtension ext opts) Toppokki.close

launchWithExtension
  :: forall (wallet :: Type)
   . WalletExt wallet
  => wallet
  -> TestOptions
  -> Aff Toppokki.Browser
launchWithExtension
  walletExt
  ( TestOptions
      { chromeExe, chromeUserDataDir, noHeadless }
  ) = Toppokki.launch
  { args:
      [ "--disable-extensions-except=" <> walletPath walletExt
      , "--load-extension=" <> walletPath walletExt
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
