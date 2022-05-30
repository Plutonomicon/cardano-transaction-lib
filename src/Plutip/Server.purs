module Plutip.Server where

import Contract.Monad
import Data.Tuple.Nested
import Effect.Aff
import Prelude
import Undefined

import Data.Newtype (over)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Path (concat, dirname, sep)

type PlutipServerConfig =
  { host :: String
  , port :: Int
  }

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces { keyId :: Int }

type PrivateKey = String

type FilePath = String

type ErrorMessage = String

type ClusterStartupParameters =
  { privateKeys :: Array (Int /\ PrivateKey {- Cbor -} )
  , nodeSocketPath :: FilePath
  , keysDirectory :: FilePath
  }

data StartClusterResponse
  = ClusterStartupFailure ClusterStartupFailureReason
  | ClusterStartupSuccess ClusterStartupParameters

data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage

startPlutipCluster :: PlutipServerConfig -> Aff StartClusterResponse
startPlutipCluster = undefined

stopPlutipCluster :: PlutipServerConfig -> Aff StopClusterResponse
stopPlutipCluster = undefined

runOnCluster
  :: forall (r :: Row Type) (a :: Type)
   . PlutipServerConfig
  -> ContractConfig r
  -> Contract r a
  -> Aff a
runOnCluster plutipCfg contractCfg contract = do
  startupResponse <- startPlutipCluster plutipCfg
  clusterContractCfg <- case startupResponse of
    ClusterStartupFailure _ -> liftEffect $ throw "Failed to start up cluster"
    ClusterStartupSuccess response -> pure $ mkClusterContractCfg contractCfg
      response
  undefined

mkClusterContractCfg
  :: forall (r :: Row Type)
   . ContractConfig r
  -> ClusterStartupParameters
  -> ContractConfig r
mkClusterContractCfg contractCfg clusterParams =
  flip (over ContractConfig) contractCfg
    \x -> x

data NetworkId = Mainnet | Nestnet Int

type CtlServerConfig =
  { port :: Int
  , nodeSocketPath :: FilePath
  , networkId :: NetworkId
  }

runCtlServer :: CtlServerConfig -> Aff Unit
runCtlServer = undefined

type OgmiosConfig =
  { port :: Int
  , nodeSocketConfig :: FilePath
  , nodeSocketPath :: FilePath
  }

mkOgmiosConfig :: OgmiosConfig -> ClusterStartupParameters -> OgmiosConfig
mkOgmiosConfig ogmiosCfg { nodeSocketPath } = ogmiosCfg
  { nodeSocketConfig = concat [ dirname nodeSocketPath, "node.config" ]
  , nodeSocketPath = nodeSocketPath
  }

runOgmios :: OgmiosConfig -> Aff Unit
runOgmios = undefined

-- { ogmiosWs :: OgmiosWebSocket
-- , datumCacheWs :: DatumCacheWebSocket
-- , serverConfig :: ServerConfig
-- , wallet :: Maybe Wallet
-- -- should probably be more tightly coupled with a wallet
-- , usedTxOuts :: UsedTxOuts
-- , networkId :: NetworkId
-- , slotConfig :: SlotConfig
-- , logLevel :: LogLevel
