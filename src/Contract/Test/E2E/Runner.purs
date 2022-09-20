-- | This module contains everything needed to run E2E tests.
-- | It is a replacement for a shell script.
module Contract.Test.E2E.Runner where

import Contract.Test.E2E.Types
import Prelude

import Contract.Test.E2E.Browser (TestOptions)
import Contract.Test.E2E.Options
  ( E2ECommand(RunE2ETests, RunBrowser, PackSettings, UnpackSettings)
  , MainOptions_
  , BrowserOptions
  )
import Contract.Test.E2E.WalletExt
  ( WalletExt(FlintExt, NamiExt, GeroExt, LodeExt, EternlExt)
  )
import Data.Array (catMaybes, mapMaybe)
import Data.Either (Either(Left, Right))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Posix.Signal (Signal(SIGINT))
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.Traversable (for_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff)
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
import Node.FS.Aff (stat)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath)
import Node.Process (lookupEnv)
import Node.Stream (onDataString)
import Prim.Row as Row
import Record.Builder (build, delete)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))
import Undefined (undefined)

type E2ETestRuntime =
  { wallets :: Map WalletExt ExtensionParams
  , chromeUserDataDir :: FilePath
  , tempDir :: FilePath
  , browserPath :: String
  , settingsArchive :: FilePath
  }

readRuntime :: BrowserOptions -> Aff E2ETestRuntime
readRuntime testOptions = do
  browserPath <- maybe findBrowser pure testOptions.chromeExe
  mbTempDir <- liftEffect (lookupEnv "E2E_TMPDIR")
  tempDir <- createTempDir mbTempDir browserPath
  chromeUserDataDir <- maybe findChromeProfile pure
    testOptions.chromeUserDataDir
  settingsArchive <- liftEffect findSettingsArchive
  liftEffect $ Console.log tempDir
  nami <- liftEffect $ readExtensionParams "NAMI"
  flint <- liftEffect $ readExtensionParams "FLINT"
  gero <- liftEffect $ readExtensionParams "GERO"
  lode <- liftEffect $ readExtensionParams "LODE"
  eternl <- liftEffect $ readExtensionParams "ETERNL"
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

initializeRuntime :: E2ETestRuntime -> Aff Unit
initializeRuntime = undefined

-- unpack wallets, settings

main :: Effect Unit
main = launchAff_ do
  -- browserPath <- findBrowser
  -- tempDir <- createTempDir mbDir browserPath
  -- chromeUserDataDir <- findChromeProfile
  -- settingsArchive <- liftEffect findSettingsArchive
  -- liftEffect $ Console.log tempDir
  -- nami <- liftEffect $ readExtensionParams "NAMI"
  -- flint <- liftEffect $ readExtensionParams "FLINT"
  -- gero <- liftEffect $ readExtensionParams "GERO"
  -- lode <- liftEffect $ readExtensionParams "LODE"
  -- eternl <- liftEffect $ readExtensionParams "ETERNL"
  -- let extensions = { nami, flint, gero, lode, eternl }
  -- runBrowser tempDir chromeUserDataDir browserPath settingsArchive extensions
  pure unit

runE2E :: E2ECommand -> Aff Unit
runE2E = case _ of
  RunE2ETests testOptions -> do
    runtime <- readRuntime $
      build (delete (Proxy :: Proxy "noHeadless")) testOptions
    pure unit
  RunBrowser browserOptions -> do
    runtime <- readRuntime browserOptions
    runBrowser runtime.tempDir runtime.chromeUserDataDir runtime.browserPath
      runtime.settingsArchive
      runtime.wallets
    pure unit
  PackSettings settingsOptions -> undefined
  UnpackSettings settingsOptions -> undefined

findSettingsArchive :: Effect SettingsArchive
findSettingsArchive =
  liftedM (error "Unable to find settings (specify E2E_SETTINGS_ARCHIVE)") $
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
    extPath ext = tempDir <> "/" <> ext.extensionId

    extensionsList :: String
    extensionsList = intercalate "," $ map extPath $ Map.values extensions
  void $ spawnAndCollectOutput browserPath
    [ "--load-extension=" <> extensionsList
    , "--user-data-dir=" <> chromeUserDataDir
    ]
    defaultSpawnOptions

extractExtension :: TempDir -> ExtensionParams -> Aff Unit
extractExtension tempDir extension = do
  void $ spawnAndCollectOutput "unzip"
    [ extension.crx
    , "-d"
    , tempDir <> "/" <> extension.extensionId
    ]
    defaultSpawnOptions

findChromeProfile :: Aff ChromeUserDataDir
findChromeProfile = do
  chromeUserDataDir <- liftedM (error "Unable to get E2E_CHROME_USER_DATA")
    $ liftEffect
    $
      lookupEnv "E2E_CHROME_USER_DATA"
  isDir <- isDirectory <$> stat chromeUserDataDir
  unless isDir do
    liftEffect $ throw $ chromeUserDataDir <>
      " is not a directory (E2E_CHROME_USER_DATA)"
  pure chromeUserDataDir

readExtensionParams :: String -> Effect (Maybe ExtensionParams)
readExtensionParams extensionName = do
  mbCrx <- lookupEnv $ extensionName <> "_CRX"
  mbPassword <- lookupEnv $ extensionName <> "_PASSWORD"
  mbExtId <- lookupEnv $ extensionName <> "_EXTID"
  case mbCrx, mbPassword, mbExtId of
    Nothing, Nothing, Nothing -> pure Nothing
    Just crx, Just password, Just extensionId -> pure $ Just
      { crx, password, extensionId }
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
    , settingsArchive
    , "./Default/IndexedDB/"
    , "./Default/Local Storage/"
    , "./Default/Extension State"
    , "./Default/Local Extension Settings"
    ]
    defaultSpawnOptions { cwd = Just chromeProfile }

unpackSettings :: SettingsArchive -> ChromeUserDataDir -> Aff Unit
unpackSettings settingsArchive chromeProfile = do
  void $ spawnAndCollectOutput "tar"
    [ "xzf"
    , settingsArchive
    ]
    defaultSpawnOptions { cwd = Just chromeProfile }

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
  child <- exec shellCmd defaultExecOptions (const $ pure unit)
  ref <- Ref.new ""
  ChildProcess.onExit child case _ of
    Normally 0 -> Ref.read ref >>= Right >>> cont
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
  -> (Either Error String -> Effect Unit)
  -> Effect Canceler
spawnAndCollectOutput_ cmd args opts cont = do
  child <- spawn cmd args opts
  ref <- Ref.new ""
  ChildProcess.onExit child $ case _ of
    Normally 0 -> do
      cont <<< Right =<< Ref.read ref
    _ -> cont $ Left $ error "Process exited with non-zero status"
  onDataString (stdout child) Encoding.UTF8
    \str -> do
      void $ Ref.modify (_ <> str) ref
  pure $ Canceler $ const $ liftEffect $ kill SIGINT child

spawnAndCollectOutput :: String -> Array String -> SpawnOptions -> Aff String
spawnAndCollectOutput cmd args opts = makeAff
  (spawnAndCollectOutput_ cmd args opts)
