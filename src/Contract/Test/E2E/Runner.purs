-- | This module contains everything needed to run E2E tests.
-- | It is a replacement for a shell script.
module Contract.Test.E2E.Runner where

import Prelude

import Contract.Test.E2E.Options
  ( BrowserOptions
  , E2ECommand(UnpackSettings, PackSettings, RunBrowser, RunE2ETests)
  , ExtensionOptions
  , SettingsOptions
  )
import Contract.Test.E2E.WalletExt
  ( ExtensionId(ExtensionId)
  , WalletExt(FlintExt, NamiExt, GeroExt, LodeExt, EternlExt)
  )
import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Either (Either(Left, Right))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String (Pattern(Pattern), trim)
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Helpers (liftedM)
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
import Node.Path (FilePath, relative)
import Node.Process (lookupEnv)
import Node.Stream (onDataString)
import Record.Builder (build, delete)
import Type.Proxy (Proxy(Proxy))
import Contract.Test.E2E.Types
  ( BrowserPath
  , ChromeUserDataDir
  , ExtensionParams
  , Extensions
  , SettingsArchive
  , TempDir
  , WalletPassword(WalletPassword)
  )

type E2ETestRuntime =
  { wallets :: Map WalletExt ExtensionParams
  , chromeUserDataDir :: FilePath
  , tempDir :: FilePath
  , browserPath :: String
  , settingsArchive :: FilePath
  }

type SettingsRuntime =
  { chromeUserDataDir :: FilePath
  , settingsArchive :: FilePath
  }

readRuntime :: BrowserOptions -> Aff E2ETestRuntime
readRuntime testOptions = do
  browserPath <- maybe findBrowser pure testOptions.chromeExe
  mbTempDir <- maybe (liftEffect lookupTempDir) (pure <<< Just)
    testOptions.tempDir
  tempDir <- createTempDir mbTempDir browserPath
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
  pure
    { browserPath
    , wallets:
        Map.fromFoldable $ catMaybes
          [ Tuple NamiExt <$> nami
          , Tuple FlintExt <$> flint
          , Tuple GeroExt <$> gero
          , Tuple LodeExt <$> lode
          , Tuple EternlExt <$> eternl
          ]
    , chromeUserDataDir
    , tempDir
    , settingsArchive
    }

ensureChromeUserDataDir :: FilePath -> Aff Unit
ensureChromeUserDataDir chromeUserDataDir = do
  liftEffect $ Console.log "hiii"
  void $ spawnAndCollectOutput "mkdir" [ "-p", chromeUserDataDir ]
    defaultSpawnOptions
    defaultErrorReader

lookupTempDir :: Effect (Maybe String)
lookupTempDir = lookupEnv "E2E_TMPDIR"

runE2E :: E2ECommand -> Aff Unit
runE2E = case _ of
  RunE2ETests testOptions -> do
    runtime <- readRuntime $
      build (delete (Proxy :: Proxy "noHeadless")) testOptions
    pure unit
  RunBrowser browserOptions -> do
    runtime <- readRuntime browserOptions
    liftEffect $ Console.log $ show runtime
    runBrowser runtime.tempDir runtime.chromeUserDataDir runtime.browserPath
      runtime.settingsArchive
      runtime.wallets
  PackSettings opts -> do
    rt <- readSettingsRuntime opts
    packSettings rt.settingsArchive rt.chromeUserDataDir
  UnpackSettings opts -> do
    rt <- readSettingsRuntime opts
    unpackSettings rt.settingsArchive rt.chromeUserDataDir

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

runBrowser
  :: TempDir
  -> ChromeUserDataDir
  -> BrowserPath
  -> SettingsArchive
  -> Extensions
  -> Aff Unit
runBrowser tempDir chromeUserDataDir browserPath settingsArchive extensions = do
  unpackSettings settingsArchive chromeUserDataDir
  for_ extensions $ extractExtension tempDir
  let
    extPath ext = tempDir <> "/" <> unwrap ext.extensionId

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
    , tempDir <> "/" <> unwrap extension.extensionId
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
  password <- map WalletPassword <$> lookupEnv (extensionName <> "_PASSWORD")
  extensionId <- map ExtensionId <$> lookupEnv (extensionName <> "_EXTID")
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
createTempDir :: Maybe FilePath -> BrowserPath -> Aff TempDir
createTempDir mbDir browserPath = do
  maybe createNew ensureExists mbDir
  where
  ensureExists dir =
    dir <$ spawnAndCollectOutput "mkdir" [ "-p", dir ] defaultSpawnOptions
      defaultErrorReader
  createNew = do
    let
      isBrowserFromSnap = String.contains (Pattern "/snap") browserPath
    uniqPart <- execAndCollectOutput "mktemp -du e2e.XXXXXXX"
    if isBrowserFromSnap then do
      void $ execAndCollectOutput $ "mkdir -p ./tmp/" <> uniqPart
      pure $ "./tmp/" <> uniqPart
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
