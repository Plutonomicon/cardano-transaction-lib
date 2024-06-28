module Ctl.Internal.Testnet.Utils
  ( find811TestnetWorkir
  , findNodeDirs
  , findTestnetPaths
  , getNodePort
  , getRuntime
  , is811TestnetDirectoryName
  , onTestnetEvent
  , parseEvent
  , readNodes
  , read872GenesisKey
  , waitFor
  , waitForTestnet872Workdir
  ) where

import Contract.Prelude

import Contract.Config as Contract.Config
import Contract.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType(PaymentSigningKeyShelleyed25519)
  , decodeTextEnvelope
  )
import Contract.Wallet.KeyFile
  ( privatePaymentKeyFromTextEnvelope
  )
import Control.Monad.Error.Class
  ( liftMaybe
  , throwError
  )
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Rec.Class (Step(Done, Loop), tailRecM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils
  ( EventSource
  , narrowEventSource
  , waitForEvent
  )
import Ctl.Internal.Testnet.Types
  ( Event(Ready872, Finished, StartupFailed)
  , GenesisUtxoKeyLocation
  , Node
  , NodeLocation
  , StartupFailure(InitializationFailed, SpawnFailed)
  , TestnetPaths
  , TestnetRuntime
  )
import Data.Array as Array
import Data.Int as Int
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (Error, error)
import Node.Encoding (Encoding(UTF8))
import Node.Encoding as Node.Encoding
import Node.FS.Sync as Node.FS
import Node.FS.Sync as Node.FS.Sync
import Node.Path (FilePath)

-- | For cardano-node 8.1.1
is811TestnetDirectoryName :: Int -> FilePath -> Boolean
is811TestnetDirectoryName n =
  isJust <<< String.stripPrefix (Pattern $ "testnet-" <> show n <> "-test-")

find811TestnetWorkir
  :: { tmpdir :: FilePath, dirIdx :: Int } -> Effect (Maybe FilePath)
find811TestnetWorkir { tmpdir, dirIdx } =
  map (tmpdir <</>> _)
    <<< Array.find (is811TestnetDirectoryName dirIdx)
    <$> Node.FS.readdir tmpdir

waitForTestnet872Workdir
  :: EventSource String -> { tmpdir :: FilePath } -> Aff { workdir :: FilePath }
waitForTestnet872Workdir src = map { workdir: _ }
  <<< waitFor src
  <<< parseTestnet872Workdir

parseTestnet872Workdir :: { tmpdir :: FilePath } -> String -> Maybe FilePath
parseTestnet872Workdir { tmpdir } = String.stripPrefix
  $ Pattern
  $ "      Workspace: "
  <> tmpdir
  <> "/"

parseEvent :: String -> Maybe Event
parseEvent = case _ of
  -- we can't know this way when 8.1.1 cardano-testnet is ready
  "    forAll109 =" -> Just Ready872
  "Usage: cardano-testnet cardano [--num-pool-nodes COUNT]" ->
    Just $ StartupFailed SpawnFailed
  "Failed to start testnet." ->
    Just $ StartupFailed InitializationFailed
  "Testnet is running.  Type CTRL-C to exit." ->
    Just Finished
  _ -> Nothing

waitFor :: forall a e. EventSource e -> (e -> Maybe a) -> Aff a
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
  for nodeDirs \{ idx, workdir, name } -> do
    let
      socketPath = testnetDirectory <</>> "socket" <</>> name
    exists <- Node.FS.exists socketPath
    unless exists
      $ throwError
      $ error
      $ "Couldn't find node socket at "
      <> socketPath
    port <- getNodePort { nodeDir: workdir }
    pure { idx, socket: socketPath, port, workdir, name }

-- | Changes TextEnvelope type to match private payment key one and tries to read that.
readTextEnvelopeAsPaymentSkey
  :: FilePath
  -> Effect Contract.Config.PrivatePaymentKey
readTextEnvelopeAsPaymentSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< Node.FS.Sync.readTextFile Node.Encoding.UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode payment skey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

parse872UtxoKeyFilename :: FilePath -> Either Error (Maybe { idx :: Int })
parse872UtxoKeyFilename path =
  traverse
    ( map { idx: _ }
        <<< note (error "Can't parse genesis key index")
        <<< Int.fromString
    )
    (String.stripPrefix (Pattern "utxo") path)

read872GenesisKeyLocations
  :: { workdir :: FilePath }
  -> Effect (Array { | GenesisUtxoKeyLocation () })
read872GenesisKeyLocations { workdir } = do
  let keysDir = workdir <</>> "utxo-keys"
  filenames <- Node.FS.readdir keysDir
  map Array.catMaybes
    $ liftEither
    $ for filenames \filename ->
        parse872UtxoKeyFilename filename <#> map \{ idx } ->
          { idx
          , path: keysDir <</>> filename <</>> "utxo.skey"
          }

read872GenesisKey
  :: forall r
   . { | GenesisUtxoKeyLocation r }
  -> Effect Contract.Config.PrivatePaymentKey
read872GenesisKey = readTextEnvelopeAsPaymentSkey <<< _.path

getNodePort :: { nodeDir :: FilePath } -> Effect UInt
getNodePort { nodeDir } =
  liftMaybe (error $ "Failed to parse port at " <> nodeDir <</>> "/port")
    <<< UInt.fromString
    =<< Node.FS.readTextFile UTF8 (nodeDir <</>> "/port")

findNodeDirs :: { workdir :: FilePath } -> Effect (Array { | NodeLocation () })
findNodeDirs { workdir } =
  Node.FS.readdir workdir <#> \subdirs ->
    flip Array.mapMaybe subdirs \dirname -> do
      idx <- Int.fromString =<< String.stripPrefix (Pattern "pools-keys/pool1")
        dirname
      pure { idx, workdir: workdir <</>> dirname, name: dirname }

findTestnetPaths
  :: { workdir :: FilePath } -> Effect (Either Error TestnetPaths)
findTestnetPaths { workdir } = runExceptT do
  let
    nodeConfigPath = workdir <</>> "configuration.yaml"
    firstNode = "socket/pool1/sock"
    nodeSocketPath = workdir <</>> firstNode
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
    throwError $ error
      $ firstNode
      <> " not found in cardano-testnet working directory."
  nodeDirs <- lift $ findNodeDirs { workdir }
  genesisKeys <- lift $ read872GenesisKeyLocations { workdir }
  pure
    { testnetDirectory: workdir
    , nodeConfigPath
    , nodeSocketPath
    , genesisKeys
    , nodeDirs
    }
