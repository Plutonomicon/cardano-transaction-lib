module Ctl.Internal.Testnet.Utils where

import Contract.Prelude

import Control.Monad.Error.Class
  ( liftMaybe
  , throwError
  )
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils
  ( EventSource
  , narrowEventSource
  , waitForEvent
  )
import Ctl.Internal.Testnet.Types
  ( Event(..)
  , Node
  , NodeLocation
  , StartupFailure(..)
  , TestnetPaths
  , TestnetRuntime
  )
import Data.Array as Array
import Data.Int as Int
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (Error, error, throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as Node.FS
import Node.Path (FilePath)

parseTestnetDirectory :: { tmpdir :: FilePath } -> String -> Maybe FilePath
parseTestnetDirectory { tmpdir } =
  map (String.takeWhile $ not <<< isFolderSeparator)
    <<< String.stripPrefix (Pattern $ tmpdir <> "/")
    <<< String.dropWhile (not <<< isFolderSeparator)
  where
  isFolderSeparator = eq $ String.codePointFromChar '/'

tellsIt'sLocation :: { tmpdir :: FilePath } -> String -> Maybe FilePath
tellsIt'sLocation tmpdir src
  | contains (Pattern "stake-pools.json") src
  , contains (Pattern "      ━━━━ File:") src =
      parseTestnetDirectory tmpdir src
  | otherwise = Nothing

parseEvent :: String -> Maybe Event
parseEvent = case _ of
  "    forAll109 =" -> Just Ready
  "Usage: cardano-testnet cardano [--num-pool-nodes COUNT]" ->
    Just $ StartupFailed SpawnFailed
  "Failed to start testnet." ->
    Just $ StartupFailed InitializationFailed
  "Testnet is running.  Type CTRL-C to exit." ->
    Just Finished
  _ -> Nothing

waitFor :: forall a. EventSource Event -> (Event -> Maybe a) -> Aff a
waitFor source f = flip tailRecM unit \_ -> do
  event <- waitForEvent source
  pure case f event of
    Just a -> Done a
    Nothing -> Loop unit

onTestnetEvent :: EventSource String -> Effect (EventSource Event)
onTestnetEvent = narrowEventSource parseEvent

getRuntime :: TestnetPaths -> Effect (Record (TestnetRuntime ()))
getRuntime paths = do
  nodes <- readNodes paths
  -- genesis <- readGenesis {workdir: paths.testnetDirectory}
  pure { nodes {-, genesis-} }

readNodes
  :: forall r
   . { nodeDirs :: Array { | NodeLocation () }
     , testnetDirectory :: FilePath
     | r
     }
  -> Effect (Array { | Node () })
readNodes { nodeDirs, testnetDirectory } = do
  for nodeDirs \{ idx, workdir } -> do
    let
      socketPath = testnetDirectory <</>> "socket" <</>> "node-spo" <> show idx
    exists <- Node.FS.exists socketPath
    unless exists
      $ throw
      $ "Couldn't find node socket at "
      <> socketPath
    port <- getNodePort { nodeDir: workdir }
    pure { idx, socket: socketPath, port, workdir }

getNodePort :: { nodeDir :: FilePath } -> Effect UInt
getNodePort { nodeDir } =
  liftMaybe (error $ "Failed to parse port at " <> nodeDir <</>> "/port")
    <<< UInt.fromString
    =<< Node.FS.readTextFile UTF8 (nodeDir <</>> "/port")

findNodeDirs :: { workdir :: FilePath } -> Effect (Array { | NodeLocation () })
findNodeDirs { workdir } = ado
  subdirs <- Node.FS.readdir workdir
  in flip Array.mapMaybe subdirs \dirname -> ado
      idx <- Int.fromString
        =<< String.stripPrefix (Pattern "node-spo") dirname
      in { idx, workdir: workdir <</>> dirname }

findTestnetPaths
  :: { workdir :: FilePath } -> Effect (Either Error TestnetPaths)
findTestnetPaths { workdir } = runExceptT do
  nodeDirs <- lift $ findNodeDirs { workdir }
  let
    nodeConfigPath = workdir <</>> "configuration.yaml"
    nodeSocketPath = workdir <</>> "socket/node-spo1"
  workdirExists <- lift $ Node.FS.exists workdir
  configPathExists <- lift $ Node.FS.exists nodeConfigPath
  socketPathExists <- lift $ Node.FS.exists nodeSocketPath
  unless workdirExists do
    throwError $ error $
      "cardano-testnet working directory not found."
  unless configPathExists do
    throwError $ error $
      "'configuration.yaml' not found in cardano-testnet working directory."
  unless socketPathExists do
    throwError $ error $
      "'socket/node-spo1' not found in cardano-testnet working directory."
  pure
    { testnetDirectory: workdir
    , nodeConfigPath
    , nodeSocketPath
    , nodeDirs
    }