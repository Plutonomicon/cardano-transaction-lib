module Ctl.Internal.Testnet.Utils where

import Contract.Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils (EventSource, onLine, waitForEvent)
import Data.Monoid.Alternate (Alternate(..))
import Data.String (Pattern(..), stripPrefix)
import Effect.Aff (Fiber, forkAff)
import Effect.Exception (Error, error)
import Internal.Testnet.Types (Event(..), StartupFailure(..), TestnetPaths)
import Node.FS.Sync as Node.FS
import Node.Path (FilePath)
import Node.Stream (Readable)

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

onTestnetEvent :: forall a. Readable a -> Effect (EventSource Event)
onTestnetEvent = flip onLine parseEvent

toAbsolutePaths :: { workdir :: FilePath } -> TestnetPaths -> TestnetPaths
toAbsolutePaths { workdir } { testnetDirectory, nodeSocketPath, nodeConfigPath } =
  { testnetDirectory: testnetAbsPath
  , nodeSocketPath: testnetAbsPath <</>> nodeSocketPath
  , nodeConfigPath: testnetAbsPath <</>> nodeConfigPath
  }
  where
  testnetAbsPath = workdir <</>> testnetDirectory

findTestnetPaths
  :: { workdir :: FilePath } -> Effect (Either Error TestnetPaths)
findTestnetPaths { workdir } = runExceptT do
  paths <- lift $ Node.FS.readdir workdir
  let
    parseTestnetDir src =
      src <$ stripPrefix (Pattern "testnet-test-") src

  testnetDirectory <- liftEither
    $ note (error "Can't find testnet-test subdirectory")
    $ unwrap
    $ foldMap (Alternate <<< parseTestnetDir) paths
  let
    absTestnetDir = workdir <</>> testnetDirectory
    nodeConfigPath = "configuration.yaml"
    nodeSocketPath = "socket/node-spo1"
  configPathExists <- lift $ Node.FS.exists $ absTestnetDir <</>> nodeConfigPath
  socketPathExists <- lift $ Node.FS.exists $ absTestnetDir <</>> nodeSocketPath
  unless configPathExists do
    throwError $ error $
      "'configuration.yaml' not found in cardano-testnet working directory."
  unless socketPathExists do
    throwError $ error $
      "'socket/node-spo1' not found in cardano-testnet working directory."
  pure
    { testnetDirectory
    , nodeConfigPath
    , nodeSocketPath
    }