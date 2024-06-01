module Ctl.Internal.Testnet.Utils where

import Contract.Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils (EventSource, narrowEventSource, waitForEvent)
import Data.String (Pattern(..), contains)
import Data.String as String
import Effect.Aff (Fiber, forkAff)
import Effect.Exception (Error, error)
import Internal.Testnet.Types (Event(..), StartupFailure(..), TestnetPaths)
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

onStartupFailure
  :: forall a
   . EventSource Event
  -> (StartupFailure -> Aff a)
  -> Aff (Fiber a)
onStartupFailure source handle = forkAff do
  err <- waitFor source case _ of
    StartupFailed err -> Just err
    _ -> Nothing
  handle err

waitFor :: forall a. EventSource Event -> (Event -> Maybe a) -> Aff a
waitFor source f = flip tailRecM unit \_ -> do
  event <- waitForEvent source
  pure case f event of
    Just a -> Done a
    Nothing -> Loop unit

onTestnetEvent :: EventSource String -> Effect (EventSource Event)
onTestnetEvent = narrowEventSource parseEvent

toAbsolutePaths :: TestnetPaths -> TestnetPaths
toAbsolutePaths { testnetDirectory, nodeSocketPath, nodeConfigPath } =
  { testnetDirectory
  , nodeSocketPath: testnetDirectory <</>> nodeSocketPath
  , nodeConfigPath: testnetDirectory <</>> nodeConfigPath
  }

findTestnetPaths
  :: { workdir :: FilePath } -> Effect (Either Error TestnetPaths)
findTestnetPaths { workdir } = runExceptT do
  let
    nodeConfigPath = "configuration.yaml"
    nodeSocketPath = "socket/node-spo1"
  workdirExists <- lift $ Node.FS.exists $ workdir
  configPathExists <- lift $ Node.FS.exists $ workdir <</>> nodeConfigPath
  socketPathExists <- lift $ Node.FS.exists $ workdir <</>> nodeSocketPath
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
    }