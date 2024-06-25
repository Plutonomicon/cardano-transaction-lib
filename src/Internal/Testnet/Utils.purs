module Ctl.Internal.Testnet.Utils
  ( findNodeDirs
  , waitForTestnet872Workdir
  , findTestnetPaths
  , getNodePort
  , getRuntime
  , is811TestnetDirectoryName
  , onTestnetEvent
  , parseEvent
  , readNodes
  , waitFor
  , read872GenesisKey
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
import Control.Alt ((<|>))
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
  , GenesisUtxoKeyLocation
  , Node
  , NodeLocation
  , StartupFailure(..)
  , TestnetPaths
  , TestnetRuntime
  )
import Data.Array as Array
import Data.Int as Int
import Data.String (Pattern(..))
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

-- type GenesisKeyFile = Int /\ FilePath

-- parseGenesisKeyFileName
--   :: FilePath
--   -> Maybe
--        (Either { vkey :: GenesisKeyFile } { skey :: GenesisKeyFile })
-- parseGenesisKeyFileName filename = do
--   idWithExt <- String.stripPrefix (Pattern "genesis") filename
--   let
--     vkey = do
--       idx <- parseIdx ".vkey" idWithExt
--       pure { vkey: idx /\ filename }
--     skey = do
--       idx <- parseIdx ".skey" idWithExt
--       pure { skey: idx /\ filename }
--   choose vkey skey
--   where
--   parseIdx ext =
--     Int.fromString <=< String.stripSuffix (Pattern ext)

-- readGenesisKeyPaths
--   :: { workdir :: FilePath }
--   -> Effect (Map.Map Int { skey :: FilePath, vkey :: FilePath })
-- readGenesisKeyPaths { workdir } = do
--   keyfiles <- Node.FS.readdir $ workdir <</>> "genesis-keys"
--   genesis <- liftMaybe (error $ "Can't parse genesis-keys filenames")
--     $ traverse parseGenesisKeyFileName keyfiles
--   let
--     empty :: forall k v. Ord k => Map.Map k v
--     empty = Map.fromFoldable []
--     vkeys /\ skeys = fold $ genesis <#>
--       either
--         (\{ vkey: elem } -> Map.fromFoldable [ elem ] /\ empty)
--         (\{ skey: elem } -> empty /\ Map.fromFoldable [ elem ])
--     keys = Map.intersectionWith { vkey: _, skey: _ } vkeys skeys
--     toFullPath filename = workdir <</>> "genesis-keys" <</>> filename

--   pure $ keys <#> \{ skey, vkey } ->
--     { skey: toFullPath skey
--     , vkey: toFullPath vkey
--     }

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
    $ String.stripSuffix (Pattern ".skey")
    =<< String.stripPrefix (Pattern "utxo") path

read872GenesisKeyLocations
  :: { workdir :: FilePath }
  -> Effect (Array { | GenesisUtxoKeyLocation () })
read872GenesisKeyLocations { workdir } = do
  let
    keysDir = workdir <</>> "utxo-keys"
  filenames <- Node.FS.readdir keysDir
  map Array.catMaybes
    $ liftEither
    $ for filenames \filename ->
        parse872UtxoKeyFilename filename <#> map \{ idx } ->
          { idx, path: keysDir <</>> filename }

read872GenesisKey
  :: forall r
   . { | GenesisUtxoKeyLocation r }
  -> Effect Contract.Config.PrivatePaymentKey
read872GenesisKey { path } = readTextEnvelopeAsPaymentSkey path

getNodePort :: { nodeDir :: FilePath } -> Effect UInt
getNodePort { nodeDir } =
  liftMaybe (error $ "Failed to parse port at " <> nodeDir <</>> "/port")
    <<< UInt.fromString
    =<< Node.FS.readTextFile UTF8 (nodeDir <</>> "/port")

findNodeDirs :: { workdir :: FilePath } -> Effect (Array { | NodeLocation () })
findNodeDirs { workdir } = ado
  subdirs <- Node.FS.readdir workdir
  in
    flip Array.mapMaybe subdirs \dirname -> ado
      idx <- Int.fromString =<< node872 dirname
      in { idx, workdir: workdir <</>> dirname, name: dirname }
  where
  node881 x =
    String.stripPrefix (Pattern "node-bft") x
      <|> String.stripPrefix (Pattern "node-pool") x
  node872 = String.stripPrefix (Pattern "node-spo")

findTestnetPaths
  :: { workdir :: FilePath } -> Effect (Either Error TestnetPaths)
findTestnetPaths { workdir } = runExceptT do
  let
    nodeConfigPath = workdir <</>> "configuration.yaml"
    firstNode811 = "socket/node-pool1"
    firstNode872 = "socket/node-spo1"
    nodeSocketPath = workdir <</>> firstNode872
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
      $ firstNode872
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