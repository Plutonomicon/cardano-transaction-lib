-- | This module contains everything needed to run E2E tests.
-- | It is a replacement for a shell script.
module Contract.Test.E2E.Runner where

import Prelude

import Contract.Test.E2E.Browser (withBrowser)
import Contract.Test.E2E.Feedback
  ( BrowserEvent(ConfirmAccess, Sign, Success, Failure)
  )
import Contract.Test.E2E.Feedback.Node (subscribeToBrowserEvents)
import Contract.Test.E2E.Helpers
  ( eternlConfirmAccess
  , eternlSign
  , flintConfirmAccess
  , flintSign
  , geroConfirmAccess
  , geroSign
  , lodeConfirmAccess
  , lodeSign
  , namiConfirmAccess
  , namiSign
  , withExample
  )
import Contract.Test.E2E.Options
  ( BrowserOptions
  , E2ECommand(UnpackSettings, PackSettings, RunBrowser, RunE2ETests)
  , ExtensionOptions
  , SettingsOptions
  , TestOptions
  )
import Contract.Test.E2E.Types
  ( BrowserPath
  , ChromeUserDataDir
  , E2ETest
  , E2ETestRuntime
  , ExtensionParams
  , Extensions
  , SettingsArchive
  , SettingsRuntime
  , TempDir
  , WalletExt(FlintExt, NamiExt, GeroExt, LodeExt, EternlExt)
  , mkE2ETest
  , mkExtensionId
  , unExtensionId
  )
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (catMaybes, nub)
import Data.Array as Array
import Data.Either (Either(Left, Right))
import Data.Foldable (fold)
import Data.List (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String (Pattern(Pattern), trim)
import Data.String as String
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Helpers (liftedM)
import Mote (group, test)
import Node.ChildProcess
  ( Exit(Normally, BySignal)
  , SpawnOptions
  , defaultExecOptions
  , defaultSpawnOptions
  , exec
  , kill
  , spawn
  , stdout
  )
import Node.ChildProcess as ChildProcess
import Node.Encoding as Encoding
import Node.FS.Aff (exists, stat)
import Node.FS.Stats (isDirectory)
import Node.Path (relative)
import Node.Process (lookupEnv)
import Node.Stream (onDataString)
import Record.Builder (build, delete)
import Test.Spec.Runner as SpecRunner
import Test.Utils as Utils
import TestM (TestPlanM)
import Type.Proxy (Proxy(Proxy))

-- | The entry point to the implementation of E2E tests.
runE2E :: E2ECommand -> Aff Unit
runE2E = case _ of
  RunE2ETests testOptions -> do
    runtime <- readTestRuntime testOptions
    tests <- liftEffect $ readTests testOptions.tests
    noHeadless <- liftEffect $ readNoHeadless testOptions.noHeadless
    runE2ETests testOptions { noHeadless = noHeadless } runtime tests
  RunBrowser browserOptions -> do
    runtime <- readBrowserRuntime browserOptions
    liftEffect $ Console.log $ show runtime
    runBrowser runtime.tempDir runtime.chromeUserDataDir runtime.browserPath
      runtime.wallets
  PackSettings opts -> do
    rt <- readSettingsRuntime opts
    packSettings rt.settingsArchive rt.chromeUserDataDir
  UnpackSettings opts -> do
    rt <- readSettingsRuntime opts
    unpackSettings rt.settingsArchive rt.chromeUserDataDir

readTestRuntime :: TestOptions -> Aff E2ETestRuntime
readTestRuntime testOptions = do
  let
    browserOptions =
      build
        ( delete (Proxy :: Proxy "noHeadless") <<<
            delete (Proxy :: Proxy "tests")
        )
        testOptions
  readBrowserRuntime browserOptions

-- | Read E2E test suite parameters from environment variables and CLI
-- | options. CLI options have higher priority.
readBrowserRuntime :: BrowserOptions -> Aff E2ETestRuntime
readBrowserRuntime testOptions = do
  browserPath <- maybe findBrowser pure testOptions.chromeExe
  tempDir <- createTempDir testOptions.tempDir browserPath
  chromeUserDataDir <- maybe findChromeProfile pure
    testOptions.chromeUserDataDir
  ensureChromeUserDataDir chromeUserDataDir
  settingsArchive <- maybe (liftEffect findSettingsArchive) pure
    testOptions.settingsArchive
  nami <- liftEffect $ readExtensionParams "NAMI"
    (Map.lookup NamiExt testOptions.wallets)
  flint <- liftEffect $ readExtensionParams "FLINT"
    (Map.lookup FlintExt testOptions.wallets)
  gero <- liftEffect $ readExtensionParams "GERO"
    (Map.lookup GeroExt testOptions.wallets)
  lode <- liftEffect $ readExtensionParams "LODE"
    (Map.lookup LodeExt testOptions.wallets)
  eternl <- liftEffect $ readExtensionParams "ETERNL"
    (Map.lookup EternlExt testOptions.wallets)
  unpackSettings settingsArchive chromeUserDataDir
  let
    wallets = Map.fromFoldable $ catMaybes
      [ Tuple NamiExt <$> nami
      , Tuple FlintExt <$> flint
      , Tuple GeroExt <$> gero
      , Tuple LodeExt <$> lode
      , Tuple EternlExt <$> eternl
      ]
  for_ wallets $ extractExtension tempDir
  pure
    { browserPath
    , wallets
    , chromeUserDataDir
    , tempDir
    , settingsArchive
    }

-- | Create ChromeUserDataDir if it does not exist
ensureChromeUserDataDir :: ChromeUserDataDir -> Aff Unit
ensureChromeUserDataDir chromeUserDataDir = do
  void $ spawnAndCollectOutput "mkdir" [ "-p", chromeUserDataDir ]
    defaultSpawnOptions
    defaultErrorReader
  void $ spawnAndCollectOutput "rm"
    [ "-f", chromeUserDataDir <> "/" <> "SingletonLock" ]
    defaultSpawnOptions
    defaultErrorReader

readTests :: Array E2ETest -> Effect (Array E2ETest)
readTests optUrls = do
  testSpecs <- lookupEnv "E2E_TEST_URLS" <#> fold
    >>> String.split (Pattern "\n")
    >>> Array.filter (String.trim >>> eq "" >>> not)
  tests <- for testSpecs \testSpec -> do
    liftMaybe (mkError testSpec) $ mkE2ETest testSpec
  pure $ nub $ optUrls <> tests
  where
  mkError testSpec =
    error $ "Failed to parse test data from: " <> testSpec <>
      "\nTest spec must be of form \"wallet:url\", where allowed wallets are: \
      \eternl, flint, gero, lode, nami."

-- | Implements `run` command
runE2ETests :: TestOptions -> E2ETestRuntime -> Array E2ETest -> Aff Unit
runE2ETests opts rt tests = do
  Utils.interpretWithConfig
    (SpecRunner.defaultConfig { timeout = pure $ wrap 500_000.0 })
    (testPlan opts rt tests)

-- | Constracts a test plan given an array of tests.
testPlan
  :: TestOptions
  -> E2ETestRuntime
  -> Array E2ETest
  -> TestPlanM (Aff Unit) Unit
testPlan opts rt@{ wallets } tests =
  group "E2E tests" do
    for_ tests \{ url, wallet } -> do
      test (walletName wallet <> ": " <> url) do
        { password, extensionId } <- liftEffect
          $ liftMaybe
              (error $ "Wallet was not provided: " <> walletName wallet)
          $ Map.lookup wallet wallets
        withBrowser opts.noHeadless rt extensionId \browser -> do
          withExample (wrap url) browser \re@{ page } -> do
            let
              confirmAccess =
                case wallet of
                  EternlExt -> eternlConfirmAccess
                  FlintExt -> flintConfirmAccess
                  GeroExt -> geroConfirmAccess
                  LodeExt -> lodeConfirmAccess
                  NamiExt -> namiConfirmAccess
              sign =
                case wallet of
                  EternlExt -> eternlSign
                  FlintExt -> flintSign
                  GeroExt -> geroSign
                  LodeExt -> lodeSign
                  NamiExt -> namiSign
              someWallet =
                { wallet
                , name: walletName wallet
                , extensionId
                , confirmAccess: confirmAccess extensionId re
                , sign: sign extensionId password re
                }
            subscribeToBrowserEvents (Just $ wrap 1000.0) page
              case _ of
                ConfirmAccess -> launchAff_ someWallet.confirmAccess
                Sign -> launchAff_ someWallet.sign
                Success -> pure unit
                Failure err -> throw err

walletName :: WalletExt -> String
walletName = case _ of
  EternlExt -> "eternl"
  FlintExt -> "flint"
  GeroExt -> "gero"
  LodeExt -> "lode"
  NamiExt -> "nami"

readNoHeadless :: Boolean -> Effect Boolean
readNoHeadless true = pure true
readNoHeadless false = do
  fromMaybe false <<< map guessBoolean <$> lookupEnv "E2E_NO_HEADLESS"
  where
  guessBoolean = case _ of
    "true" -> true
    _ -> false

readSettingsRuntime :: SettingsOptions -> Aff SettingsRuntime
readSettingsRuntime { chromeUserDataDir, settingsArchive } = do
  d <- maybe findChromeProfile pure chromeUserDataDir
  a <- maybe (liftEffect findSettingsArchive) pure settingsArchive
  pure { settingsArchive: a, chromeUserDataDir: d }

findSettingsArchive :: Effect SettingsArchive
findSettingsArchive =
  liftedM
    ( error
        "Unable to find settings archive (specify E2E_SETTINGS_ARCHIVE or --settings-archive)"
    ) $
    lookupEnv "E2E_SETTINGS_ARCHIVE"

-- | Implements `browser` command.
runBrowser
  :: TempDir
  -> ChromeUserDataDir
  -> BrowserPath
  -> Extensions
  -> Aff Unit
runBrowser tempDir chromeUserDataDir browserPath extensions = do
  let
    extPath ext = tempDir <> "/" <> unExtensionId ext.extensionId

    extensionsList :: String
    extensionsList = intercalate "," $ map extPath $ Map.values extensions
  void $ spawnAndCollectOutput browserPath
    [ "--load-extension=" <> extensionsList
    , "--user-data-dir=" <> chromeUserDataDir
    ]
    defaultSpawnOptions
    defaultErrorReader

extractExtension :: TempDir -> ExtensionParams -> Aff Unit
extractExtension tempDir extension = do
  void $ spawnAndCollectOutput "unzip"
    [ extension.crx
    , "-d"
    , tempDir <> "/" <> unExtensionId extension.extensionId
    ]
    defaultSpawnOptions
    errorReader
  where
  errorReader = case _ of
    Normally 0 -> Nothing
    Normally 1 -> Nothing
    Normally code -> Just $ "(code: " <> show code <> ")"
    BySignal signal -> Just $ show signal

findChromeProfile :: Aff ChromeUserDataDir
findChromeProfile = do
  chromeUserDataDir <- liftedM (error "Unable to get E2E_CHROME_USER_DATA")
    $ liftEffect
    $
      lookupEnv "E2E_CHROME_USER_DATA"
  doesExist <- exists chromeUserDataDir
  unless doesExist do
    ensureChromeUserDataDir chromeUserDataDir
  isDir <- isDirectory <$> stat chromeUserDataDir
  unless isDir do
    liftEffect $ throw $ chromeUserDataDir <>
      " is not a directory (E2E_CHROME_USER_DATA)"
  pure chromeUserDataDir

readExtensionParams
  :: String -> Maybe ExtensionOptions -> Effect (Maybe ExtensionParams)
readExtensionParams extensionName mbCliOptions = do
  crxFile <- lookupEnv $ extensionName <> "_CRX"
  password <- lookupEnv (extensionName <> "_PASSWORD")
  mbExtensionIdStr <- lookupEnv (extensionName <> "_EXTID")
  extensionId <- for mbExtensionIdStr \str ->
    liftMaybe (error $ mkExtIdError str) $ mkExtensionId str
  let
    envOptions :: ExtensionOptions
    envOptions = { crxFile, password, extensionId }

    mergedOptions :: ExtensionOptions
    mergedOptions = case mbCliOptions of
      Nothing -> envOptions
      Just cliOptions -> mergeExtensionOptions cliOptions envOptions
  toExtensionParams mergedOptions
  where
  mergeExtensionOptions
    :: ExtensionOptions -> ExtensionOptions -> ExtensionOptions
  mergeExtensionOptions a b =
    { crxFile: a.crxFile <|> b.crxFile
    , password: a.password <|> b.password
    , extensionId: a.extensionId <|> b.extensionId
    }

  toExtensionParams :: ExtensionOptions -> Effect (Maybe ExtensionParams)
  toExtensionParams { crxFile, password, extensionId } =
    case crxFile, password, extensionId of
      Nothing, Nothing, Nothing -> pure Nothing
      Just crx, Just pwd, Just extId -> pure $ Just
        { crx, password: pwd, extensionId: extId }
      _, _, _ -> throw $ "Please ensure that either none or all of"
        <> extensionName
        <> "_CRX, "
        <> extensionName
        <> "_PASSWORD and "
        <> extensionName
        <> "_EXTID are provided"
  mkExtIdError str =
    "Unable to parse extension ID. must be a string consisting of 32 characters\
    \, got: " <> str

packSettings :: SettingsArchive -> ChromeUserDataDir -> Aff Unit
packSettings settingsArchive chromeProfile = do
  void $ spawnAndCollectOutput "tar"
    [ "czf"
    , relative chromeProfile settingsArchive
    , "./Default/IndexedDB/"
    , "./Default/Local Storage/"
    , "./Default/Extension State"
    , "./Default/Local Extension Settings"
    ]
    defaultSpawnOptions { cwd = Just chromeProfile }
    defaultErrorReader

unpackSettings :: SettingsArchive -> ChromeUserDataDir -> Aff Unit
unpackSettings settingsArchive chromeProfile = do
  void $ spawnAndCollectOutput "tar"
    [ "xzf"
    , relative chromeProfile settingsArchive
    ]
    defaultSpawnOptions { cwd = Just chromeProfile }
    defaultErrorReader

findBrowser :: Aff BrowserPath
findBrowser =
  liftEffect (lookupEnv "E2E_BROWSER") >>=
    maybe tryBrowserBinaries pure
  where
  tryBrowserBinaries = do
    res <- execAndCollectOutput "which chromium google-chrome | head -n1"
    when (res == "") do
      liftEffect $ throw $
        "Unable to find chromium or google-chrome binaries. Set E2E_BROWSER environment variable manually"
    pure res

-- | Find a suitable temp directory for E2E tests. Apps installed with `snap`
-- | don't work in $E2E_TMPDIR, because of lacking read access.
createTempDir :: Maybe TempDir -> BrowserPath -> Aff TempDir
createTempDir mbOptionsTempDir browserPath = do
  mbTempDir <- maybe (liftEffect $ lookupEnv "E2E_TMPDIR") (pure <<< Just)
    mbOptionsTempDir
  for_ mbTempDir ensureExists
  maybe createNew createNewSubdir mbTempDir
  where
  ensureExists dir =
    dir <$ spawnAndCollectOutput "mkdir" [ "-p", dir ] defaultSpawnOptions
      defaultErrorReader
  createNewSubdir prefix = do
    uniqPart <- execAndCollectOutput "mktemp -du e2e.XXXXXXX"
    void $ spawnAndCollectOutput "mkdir" [ "-p", prefix <> "/" <> uniqPart ]
      defaultSpawnOptions
      defaultErrorReader
    pure $ prefix <> "/" <> uniqPart
  createNew = do
    realPath <- spawnAndCollectOutput "which" [ browserPath ]
      defaultSpawnOptions
      defaultErrorReader
    let
      isBrowserFromSnap = String.contains (Pattern "/snap") realPath
    uniqPart <- execAndCollectOutput "mktemp -du e2e.XXXXXXX"
    if isBrowserFromSnap then do
      liftEffect $ throw $
        "Your browser is installed from Snap store: " <> realPath
          <> ". Because of that it can't access temporary directory. Please "
          <> "provide E2E_TMPDIR variable or use --tmp-dir CLI argument"
    else do
      prefix <- execAndCollectOutput "mktemp -d"
      void $ execAndCollectOutput $ "mkdir -p " <> prefix <> "/" <> uniqPart
      pure $ prefix <> "/" <> uniqPart

execAndCollectOutput_
  :: String
  -> (Either Error String -> Effect Unit)
  -> Effect Canceler
execAndCollectOutput_ shellCmd cont = do
  Console.log $ shellCmd
  child <- exec shellCmd defaultExecOptions (const $ pure unit)
  ref <- Ref.new ""
  ChildProcess.onExit child case _ of
    Normally 0 -> Ref.read ref >>= trim >>> Right >>> cont
    exitStatus -> do
      output <- Ref.read ref
      cont $ Left
        ( error $ "Command failed: " <> shellCmd <> " (" <> show exitStatus
            <> "), output collected so far: "
            <>
              output
        )
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      void $ Ref.modify (_ <> str) ref
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

execAndCollectOutput :: String -> Aff String
execAndCollectOutput cmd = makeAff (execAndCollectOutput_ cmd)

spawnAndCollectOutput_
  :: String
  -> Array String
  -> SpawnOptions
  -> (Exit -> Maybe String)
  -> (Either Error String -> Effect Unit)
  -> Effect Canceler
spawnAndCollectOutput_ cmd args opts errorReader cont = do
  Console.log $ cmd
  Console.log $ show args
  child <- spawn cmd args opts
  ref <- Ref.new ""
  ChildProcess.onExit child $ errorReader >>> case _ of
    Nothing -> do
      cont <<< Right <<< trim =<< Ref.read ref
    Just errorStr -> do
      output <- Ref.read ref
      cont $ Left $ error $
        "Process exited with non-zero status (" <> errorStr
          <> "). Output collected so far: "
          <> output
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      void $ Ref.modify (_ <> str) ref
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

spawnAndCollectOutput
  :: String
  -> Array String
  -> SpawnOptions
  -> (Exit -> Maybe String)
  -> Aff String
spawnAndCollectOutput cmd args opts errorReader = makeAff
  (spawnAndCollectOutput_ cmd args opts errorReader)

defaultErrorReader :: Exit -> Maybe String
defaultErrorReader =
  case _ of
    Normally 0 -> Nothing
    exitStatus -> Just $ show exitStatus
