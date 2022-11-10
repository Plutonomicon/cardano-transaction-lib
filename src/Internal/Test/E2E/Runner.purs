-- | This module contains everything needed to run E2E tests.
module Ctl.Internal.Test.E2E.Runner
  ( runE2ECommand
  , runE2ETests
  ) where

import Prelude

import Affjax (defaultRequest, request) as Affjax
import Affjax (printError)
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Control.Promise (Promise, toAffE)
import Ctl.Internal.Helpers (liftedM)
import Ctl.Internal.Test.E2E.Browser (withBrowser)
import Ctl.Internal.Test.E2E.Feedback
  ( BrowserEvent(ConfirmAccess, Sign, Success, Failure)
  )
import Ctl.Internal.Test.E2E.Feedback.Node (subscribeToBrowserEvents)
import Ctl.Internal.Test.E2E.Options
  ( BrowserOptions
  , E2ECommand(UnpackSettings, PackSettings, RunBrowser, RunE2ETests)
  , ExtensionOptions
  , SettingsOptions
  , TestOptions
  )
import Ctl.Internal.Test.E2E.Types
  ( Browser
  , ChromeUserDataDir
  , E2ETest
  , E2ETestRuntime
  , ExtensionParams
  , Extensions
  , RunningE2ETest
  , SettingsArchive
  , SettingsRuntime
  , TmpDir
  , WalletExt(NamiExt, LodeExt, GeroExt, FlintExt, EternlExt)
  , mkE2ETest
  , mkExtensionId
  , unExtensionId
  )
import Ctl.Internal.Test.E2E.Wallets
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
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpretWithConfig)
import Data.Array (catMaybes, mapMaybe, nub)
import Data.Array as Array
import Data.Either (Either(Right, Left))
import Data.Foldable (fold)
import Data.HTTP.Method (Method(GET))
import Data.Int as Int
import Data.List (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String (Pattern(Pattern), toLower, toUpper, trim)
import Data.String as String
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Mote (group, test)
import Node.Buffer (fromArrayBuffer)
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
import Node.FS.Aff (exists, stat, writeFile)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, concat, dirname, relative)
import Node.Process (lookupEnv)
import Node.Stream (onDataString)
import Record.Builder (build, delete)
import Test.Spec.Runner as SpecRunner
import Toppokki as Toppokki
import Type.Proxy (Proxy(Proxy))

-- | The entry point to the implementation of E2E tests.
runE2ECommand :: E2ECommand -> Aff Unit
runE2ECommand = case _ of
  RunE2ETests testOptions -> do
    runtime <- readTestRuntime testOptions
    tests <- liftEffect $ readTests testOptions.tests
    noHeadless <- liftEffect $ readNoHeadless testOptions.noHeadless
    testTimeout <- liftEffect $ readTestTimeout testOptions.testTimeout
    let
      testOptions' = testOptions
        { noHeadless = noHeadless, testTimeout = testTimeout, tests = tests }
    runE2ETests testOptions' runtime
  RunBrowser browserOptions -> do
    runtime <- readBrowserRuntime Nothing browserOptions
    runBrowser runtime.tmpDir runtime.chromeUserDataDir runtime.browser
      runtime.wallets
  PackSettings opts -> do
    rt <- readSettingsRuntime opts
    packSettings rt.settingsArchive rt.chromeUserDataDir
  UnpackSettings opts -> do
    rt <- readSettingsRuntime opts
    unpackSettings rt.settingsArchive rt.chromeUserDataDir

ensureDir :: FilePath -> Aff Unit
ensureDir dir = do
  dirExists <- exists dir
  unless dirExists $ do
    liftEffect $ log $ "Creating directory " <> dir
    void $ spawnAndCollectOutput "mkdir" [ "-p", dir ]
      defaultSpawnOptions
      defaultErrorReader

-- | Implements `run` command
runE2ETests :: TestOptions -> E2ETestRuntime -> Aff Unit
runE2ETests opts rt = do
  interpretWithConfig
    ( SpecRunner.defaultConfig
        { timeout = Milliseconds <<< mul 1000.0 <<< Int.toNumber <$>
            opts.testTimeout
        }
    )
    (testPlan opts rt opts.tests)

-- | Constructs a test plan given an array of tests.
testPlan
  :: TestOptions
  -> E2ETestRuntime
  -> Array E2ETest
  -> TestPlanM (Aff Unit) Unit
testPlan opts rt@{ wallets } tests =
  group "E2E tests" do
    for_ tests case _ of
      { url, wallet: Nothing } -> do
        test url do
          withBrowser opts.noHeadless rt Nothing \browser -> do
            withE2ETest (wrap url) browser \{ page } -> do
              subscribeToBrowserEvents (Just $ wrap 1000.0) page
                case _ of
                  Success -> pure unit
                  Failure err -> throw err
                  _ -> pure unit
      { url, wallet: Just wallet } -> do
        test (walletName wallet <> ": " <> url) do
          { password, extensionId } <- liftEffect
            $ liftMaybe
                (error $ "Wallet was not provided: " <> walletName wallet)
            $ Map.lookup wallet wallets
          withBrowser opts.noHeadless rt (Just extensionId) \browser -> do
            withE2ETest (wrap url) browser \re@{ page } -> do
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

-- | Implements `browser` command.
runBrowser
  :: TmpDir
  -> ChromeUserDataDir
  -> Browser
  -> Extensions
  -> Aff Unit
runBrowser tmpDir chromeUserDataDir browser extensions = do
  let
    extPath ext = tmpDir <> "/" <> unExtensionId ext.extensionId

    extensionsList :: String
    extensionsList = intercalate "," $ map extPath $ Map.values extensions
  void $ spawnAndCollectOutput browser
    [ "--load-extension=" <> extensionsList
    , "--user-data-dir=" <> chromeUserDataDir
    ]
    defaultSpawnOptions
    defaultErrorReader

readTestRuntime :: TestOptions -> Aff E2ETestRuntime
readTestRuntime testOptions = do
  let
    browserOptions =
      build
        ( delete (Proxy :: Proxy "noHeadless")
            <<< delete (Proxy :: Proxy "tests")
            <<<
              delete (Proxy :: Proxy "testTimeout")
        )
        testOptions
  readBrowserRuntime Nothing browserOptions

readExtensions
  :: Map.Map WalletExt ExtensionOptions
  -> Aff (Map.Map WalletExt ExtensionParams)
readExtensions wallets = do
  nami <- readExtensionParams "NAMI" wallets
  flint <- readExtensionParams "FLINT" wallets
  gero <- readExtensionParams "GERO" wallets
  lode <- readExtensionParams "LODE" wallets
  eternl <- readExtensionParams "ETERNL" wallets

  pure $ Map.fromFoldable $ catMaybes
    [ Tuple NamiExt <$> nami
    , Tuple FlintExt <$> flint
    , Tuple GeroExt <$> gero
    , Tuple LodeExt <$> lode
    , Tuple EternlExt <$> eternl
    ]

-- | Read E2E test suite parameters from environment variables and CLI
-- | options. CLI options have higher priority.
readBrowserRuntime
  :: Maybe (Array E2ETest) -> BrowserOptions -> Aff E2ETestRuntime
readBrowserRuntime mbTests testOptions = do
  browser <- maybe findBrowser pure testOptions.browser
  tmpDir <- createTmpDir testOptions.tmpDir browser

  chromeUserDataDir <- findChromeProfile settingsOptions
  ensureChromeUserDataDir chromeUserDataDir
  settingsArchive <- findSettingsArchive settingsOptions
  unpackSettings settingsArchive chromeUserDataDir

  wallets <- readExtensions testOptions.wallets

  let
    runtime =
      { browser
      , wallets
      , chromeUserDataDir
      , tmpDir
      , settingsArchive
      }

  -- Must be executed before extractExtension call
  for_ (sanityCheck mbTests runtime) (throw >>> liftEffect)
  for_ wallets $ extractExtension tmpDir
  pure runtime
  where
  settingsOptions = build
    ( delete (Proxy :: Proxy "wallets")
        <<< delete (Proxy :: Proxy "tmpDir")
        <<< delete (Proxy :: Proxy "browser")
    )
    testOptions

-- | Check that the provided set of options is valid in a given runtime.
sanityCheck :: Maybe (Array E2ETest) -> E2ETestRuntime -> Maybe String
sanityCheck mbTests { wallets } =
  case errors of
    [] -> Nothing
    _ -> Just $ Array.intercalate "\n" $ nub errors
  where
  errors = walletErrors <> testErrors <> walletIdDuplicateErrors
  testErrors
    | Just tests <- mbTests
    , Array.length tests == 0 =
        [ "No tests to run! Use E2E_TESTS or --test to specify some." ]
    | otherwise = []
  -- Check that all extension IDs are different
  walletIdDuplicateErrors =
    let
      allIds = Array.fromFoldable $ Map.values wallets <#> _.extensionId
      uniqIds = nub allIds
      diff = Array.difference allIds uniqIds
    in
      if Array.length allIds /= Array.length uniqIds then
        [ "Some of the provided extension IDs are duplicate: " <>
            (intercalate ", " $ diff <#> unExtensionId)
        ]
      else []
  -- check that all required wallet extensions are provided
  walletErrors
    | Just tests <- mbTests =
        tests `flip mapMaybe` \test ->
          test.wallet >>= \wallet ->
            case Map.lookup wallet wallets of
              Just _ -> Nothing
              Nothing ->
                let
                  name = walletName wallet
                  capName = String.toUpper name
                in
                  Just $ "Wallet " <> name
                    <> " was not provided! Please specify "
                    <> capName
                    <> "_CRX, "
                    <> capName
                    <> "_PASSWORD, "
                    <> capName
                    <> "_EXTID "
                    <> "or "
                    <> "--"
                    <> name
                    <> "-crx, "
                    <> "--"
                    <> name
                    <> "-password, "
                    <> "--"
                    <> name
                    <> "-extid"
    | otherwise = []

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
  testSpecs <- lookupEnv "E2E_TESTS" <#> fold
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

-- | Run an example in a new browser page.
-- |
-- | Example usage:
-- |
-- | ```purescript
-- |   withBrowser options NamiExt \browser -> do
-- |     withE2ETest
-- |        (wrap "http://myserver:1234/docontract")
-- |        browser do
-- |          namiSign $ wrap "mypassword"
-- | ```
withE2ETest
  :: forall (a :: Type)
   . Toppokki.URL
  -> Toppokki.Browser
  -> (RunningE2ETest -> Aff a)
  -> Aff a
withE2ETest url browser action = do
  startExample url browser >>= action

-- | Navigate to an example's page, inject jQuery and set up error handlers
startExample :: Toppokki.URL -> Toppokki.Browser -> Aff RunningE2ETest
startExample url browser = do
  page <- Toppokki.newPage browser
  jQuery <- retrieveJQuery page
  Toppokki.goto url page
  pure { browser, jQuery, page }

-- | Download jQuery
retrieveJQuery :: Toppokki.Page -> Aff String
retrieveJQuery = toAffE <<< _retrieveJQuery

foreign import _retrieveJQuery :: Toppokki.Page -> Effect (Promise String)

readNoHeadless :: Boolean -> Effect Boolean
readNoHeadless true = pure true
readNoHeadless false = do
  mbStr <- lookupEnv "E2E_NO_HEADLESS"
  case mbStr of
    Nothing -> pure false
    Just str -> do
      liftMaybe (error $ "Failed to read E2E_NO_HEADLESS: " <> str) $
        readBoolean str
  where
  readBoolean = case _ of
    "true" -> Just true
    "false" -> Just false
    _ -> Nothing

readTestTimeout :: Maybe Int -> Effect (Maybe Int)
readTestTimeout r@(Just _) = pure r
readTestTimeout Nothing = do
  mbTimeoutStr <- lookupEnv "E2E_TEST_TIMEOUT"
  for mbTimeoutStr \timeoutStr -> do
    case Int.fromString timeoutStr of
      Nothing -> throw $ "Unable to decode E2E_TEST_TIMEOUT: " <> timeoutStr
      Just timeout -> pure timeout

readSettingsRuntime :: SettingsOptions -> Aff SettingsRuntime
readSettingsRuntime testOptions = do
  d <- findChromeProfile testOptions
  a <- findSettingsArchive testOptions
  pure { settingsArchive: a, chromeUserDataDir: d }

extractExtension :: TmpDir -> ExtensionParams -> Aff Unit
extractExtension tmpDir extension = do
  void $ spawnAndCollectOutput "unzip"
    [ extension.crx
    , "-d"
    , tmpDir <> "/" <> unExtensionId extension.extensionId
    ]
    defaultSpawnOptions
    errorReader
  where
  errorReader = case _ of
    Normally 0 -> Nothing
    Normally 1 -> Nothing
    Normally code -> Just $ "(code: " <> show code <> ")"
    BySignal signal -> Just $ show signal

findSettingsArchive :: SettingsOptions -> Aff SettingsArchive
findSettingsArchive testOptions = do
  settingsArchive <-
    maybe
      ( liftedM
          ( error
              "Please specify --settings-archive option or ensure E2E_SETTINGS_ARCHIVE is set"
          ) $ liftEffect $ lookupEnv "E2E_SETTINGS_ARCHIVE"
      )
      pure $ testOptions.chromeUserDataDir

  doesExist <- exists settingsArchive

  unless doesExist $ do
    liftEffect $ log $ settingsArchive <> " does not exist."
    settingsArchiveUrl <-
      maybe
        ( liftedM
            ( error
                "Please specify --settings-archive-url option or ensure E2E_SETTINGS_ARCHIVE_URL is set"
            ) $ liftEffect $ lookupEnv "E2E_SETTINGS_ARCHIVE_URL"
        )
        pure $ testOptions.settingsArchiveUrl
    liftEffect $ log $ "Downloading settings archive from " <>
      settingsArchiveUrl
    downloadTo settingsArchiveUrl settingsArchive
  pure settingsArchive

findChromeProfile
  :: SettingsOptions -> Aff ChromeUserDataDir
findChromeProfile testOptions = do
  chromeDataDir <-
    maybe
      ( liftedM
          ( error
              "Please specify any of --chrome-user-data or E2E_CHROME_USER_DATA"
          ) $ liftEffect $ lookupEnv "E2E_CHROME_USER_DATA"
      )
      pure $ testOptions.chromeUserDataDir

  doesExist <- exists chromeDataDir
  unless doesExist $
    ensureChromeUserDataDir chromeDataDir
  isDir <- isDirectory <$> stat chromeDataDir
  unless isDir
    $ liftEffect
    $ throw
    $ "Chrome user data directory " <> chromeDataDir <> " is not a directory."
  pure chromeDataDir

findBrowser :: Aff Browser
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

readExtensionParams
  :: String
  -> Map.Map WalletExt ExtensionOptions
  -> Aff (Maybe ExtensionParams)
readExtensionParams extensionName wallets = do
  crxFile <- liftEffect $ lookupEnv $ extensionName <> "_CRX"
  crxUrl <- liftEffect $ lookupEnv (extensionName <> "_CRX_URL")
  password <- liftEffect $ lookupEnv (extensionName <> "_PASSWORD")
  mbExtensionIdStr <- liftEffect $ lookupEnv (extensionName <> "_EXTID")
  extensionId <- for mbExtensionIdStr \str ->
    liftMaybe (error $ mkExtIdError str) $ mkExtensionId str
  let
    mbCliOptions :: Maybe ExtensionOptions
    mbCliOptions = Map.lookup NamiExt wallets

    envOptions :: ExtensionOptions
    envOptions = { crxFile, password, extensionId, crxUrl }

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
    , crxUrl: a.crxUrl <|> b.crxUrl
    }

  toExtensionParams :: ExtensionOptions -> Aff (Maybe ExtensionParams)
  toExtensionParams { crxFile, password, extensionId, crxUrl } =
    case crxFile, password, extensionId of
      Nothing, Nothing, Nothing -> pure Nothing
      Just crx, Just pwd, Just extId -> do
        doesExist <- exists crx
        unless doesExist $ do
          liftEffect $ log $ crx <> " does not exist."
          crxFileUrl <-
            maybe
              ( liftedM
                  ( error $ "Please specify  --" <> toLower extensionName
                      <> "-crx-url or ensure"
                      <> toUpper extensionName
                      <> "_CRX_URL is set"
                  ) $ liftEffect $ lookupEnv $ extensionName <> "_CRX_URL"
              )
              pure $ crxUrl
          liftEffect $ log $ "Downloading from " <> crxFileUrl
          downloadTo crxFileUrl crx
        pure $ Just { crx, password: pwd, extensionId: extId }
      _, _, _ -> liftEffect $ throw $ "Please ensure that either none or all of"
        <> extensionName
        <> "_CRX, "
        <> extensionName
        <> "_PASSWORD and "
        <> extensionName
        <> "_EXTID are provided"

  mkExtIdError str =
    "Unable to parse extension ID. must be a string consisting of 32 characters\
    \, got: " <> str

-- | Pack user data directory to an archive
packSettings :: SettingsArchive -> ChromeUserDataDir -> Aff Unit
packSettings settingsArchive userDataDir = do
  -- Passing a non-existent directory to tar will error,
  -- but we can't rely on the existence of these directories.
  paths <- filterExistingPaths userDataDir
    [ "./Default/IndexedDB/"
    , "./Default/Local Storage/"
    , "./Default/Extension State"
    , "./Default/Local Extension Settings"
    ]
  case paths of
    [] -> do
      -- Create an empty tar.gz
      void $ spawnAndCollectOutput "tar"
        [ "czf"
        , relative userDataDir settingsArchive
        , "-T"
        , "/dev/null"
        ]
        defaultSpawnOptions { cwd = Just userDataDir }
        defaultErrorReader
    _ -> do
      void $ spawnAndCollectOutput "tar"
        ( [ "czf"
          , relative userDataDir settingsArchive
          ] <> paths
        )
        defaultSpawnOptions { cwd = Just userDataDir }
        defaultErrorReader

-- | Filter out non-existing paths, relative to the given directory
filterExistingPaths :: FilePath -> Array FilePath -> Aff (Array FilePath)
filterExistingPaths base paths = do
  catMaybes <$> for paths \path -> do
    exists (concat [ base, path ]) >>= case _ of
      false -> pure Nothing
      true -> pure $ Just path

-- | Unpack settings archive to user data directory
unpackSettings :: SettingsArchive -> ChromeUserDataDir -> Aff Unit
unpackSettings settingsArchive userDataDir = do
  void $ spawnAndCollectOutput "tar"
    [ "xzf"
    , relative userDataDir settingsArchive
    ]
    defaultSpawnOptions { cwd = Just userDataDir }
    defaultErrorReader

-- | Find a suitable temp directory for E2E tests. Apps installed with `snap`
-- | don't work in $E2E_TMPDIR, because of lacking read access.
createTmpDir :: Maybe TmpDir -> Browser -> Aff TmpDir
createTmpDir mbOptionsTmpDir browser = do
  mbTmpDir <- maybe (liftEffect $ lookupEnv "E2E_TMPDIR") (pure <<< Just)
    mbOptionsTmpDir
  for_ mbTmpDir ensureExists
  maybe createNew createNewSubdir mbTmpDir
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
    realPath <- spawnAndCollectOutput "which" [ browser ]
      defaultSpawnOptions
      defaultErrorReader
    let
      isBrowserFromSnap = String.contains (Pattern "/snap") realPath
    uniqPart <- execAndCollectOutput "mktemp -du e2e.XXXXXXX"
    if isBrowserFromSnap then do
      liftEffect $ throw $
        "Your browser is installed from Snap store: " <> realPath
          <> ". Because of that it can't access the temporary directory. Please"
          <> " provide E2E_TMPDIR variable or use --tmp-dir CLI argument"
    else do
      prefix <- execAndCollectOutput "mktemp -d"
      void $ execAndCollectOutput $ "mkdir -p " <> prefix <> "/" <> uniqPart
      pure $ prefix <> "/" <> uniqPart

execAndCollectOutput_
  :: String
  -> (Either Error String -> Effect Unit)
  -> Effect Canceler
execAndCollectOutput_ shellCmd cont = do
  child <- exec shellCmd defaultExecOptions (const $ pure unit)
  ref <- Ref.new ""
  ChildProcess.onExit child case _ of
    Normally 0 -> Ref.read ref >>= trim >>> Right >>> cont
    exitStatus -> do
      output <- Ref.read ref
      cont $ Left
        ( error $ "Command failed: " <> shellCmd <> " (" <> show exitStatus
            <> ")."
            <>
              if String.null output then ""
              else " Output collected so far: " <> output
        )
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      void $ Ref.modify (_ <> str) ref
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

-- | Run a shell command and collect the output.
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
  child <- spawn cmd args opts
  ref <- Ref.new ""
  ChildProcess.onExit child $ errorReader >>> case _ of
    Nothing -> do
      cont <<< Right <<< trim =<< Ref.read ref
    Just errorStr -> do
      output <- Ref.read ref
      cont $ Left $ error $
        "Process `" <> cmd <> " " <> intercalate " " args
          <> "` exited with non-zero status ("
          <> errorStr
          <> ")."
          <>
            if String.null output then ""
            else " Output collected so far: " <> output
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      void $ Ref.modify (_ <> str) ref
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

-- | Spawn a command with CLI parameters. The last arguments allows to treat
-- | various non-zero exit-codes as a norm (and avoid throwing an error).
spawnAndCollectOutput
  :: String
  -> Array String
  -> SpawnOptions
  -> (Exit -> Maybe String)
  -> Aff String
spawnAndCollectOutput cmd args opts errorReader = makeAff
  (spawnAndCollectOutput_ cmd args opts errorReader)

downloadTo :: String -> FilePath -> Aff Unit
downloadTo url filePath = do
  ensureDir $ dirname filePath
  eRes <- Affjax.request Affjax.defaultRequest
    { method = Left GET
    , url = url
    , responseFormat = Affjax.ResponseFormat.arrayBuffer
    , headers = []
    }

  case eRes of
    Left err -> do
      liftEffect $ log $ "HTTP Request error: " <> printError err
      pure unit
    Right res -> do
      buf <- liftEffect $ fromArrayBuffer res.body
      writeFile filePath buf
      pure unit

defaultErrorReader :: Exit -> Maybe String
defaultErrorReader =
  case _ of
    Normally 0 -> Nothing
    exitStatus -> Just $ show exitStatus

walletName :: WalletExt -> String
walletName = case _ of
  EternlExt -> "eternl"
  FlintExt -> "flint"
  GeroExt -> "gero"
  LodeExt -> "lode"
  NamiExt -> "nami"
