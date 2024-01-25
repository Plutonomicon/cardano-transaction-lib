module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Launcher.Node (nodeSocketFile)

import Control.Concurrent.MVar (isEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Plutip.Cluster (startFundedCluster, stopCluster)
import Plutip.Config (
  ExtraConfig (ExtraConfig, ecEpochSize, ecMaxTxSize, ecSlotLength),
  PlutipConfig (extraConfig),
  ecRaiseExUnitsToMax,
 )
import Plutip.Keys (signKeyCBORHex)
import Plutip.Types (ClusterEnv (runningNode), RunningNode (RunningNode), keysDir)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Types (
  AppM,
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
    NegativeLovelaces,
    NodeConfigNotFound
  ),
  ClusterStartupParameters (
    ClusterStartupParameters,
    keysDirectory,
    nodeConfigPath,
    nodeSocketPath,
    privateKeys
  ),
  Env (status),
  Lovelace (unLovelace),
  ServerOptions,
  StartClusterRequest (
    StartClusterRequest,
    epochSize,
    keysToGenerate,
    maxTxSize,
    raiseExUnitsToMax,
    slotLength
  ),
  StartClusterResponse (
    ClusterStartupFailure,
    ClusterStartupSuccess
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterFailure, StopClusterSuccess),
 )

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  _
  StartClusterRequest
    { keysToGenerate
    , slotLength
    , epochSize
    , maxTxSize
    , raiseExUnitsToMax
    } = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \lovelaceAmounts ->
      for_ lovelaceAmounts $ \lovelaces ->
        unless (unLovelace lovelaces > 0) $
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let cfg = def {extraConfig = extraConf}
        keysToGenerate' = map fromIntegral <$> keysToGenerate
    (statusTVar, (clusterEnv, keys)) <- liftIO $ startFundedCluster cfg keysToGenerate' (curry pure)
    liftIO $ putMVar statusMVar statusTVar

    let nodeConfigPath = getNodeConfigFile clusterEnv
    -- safeguard against directory tree structure changes
    unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
    pure $
      ClusterStartupSuccess $
        ClusterStartupParameters
          { privateKeys = signKeyCBORHex <$> keys
          , nodeSocketPath = getNodeSocketFile clusterEnv
          , nodeConfigPath = nodeConfigPath
          , keysDirectory = keysDir clusterEnv
          }
    where
      getNodeSocketFile (runningNode -> RunningNode conn _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

      extraConf :: ExtraConfig
      extraConf =
        let defConfig = def
         in ExtraConfig
              (fromMaybe (ecSlotLength defConfig) slotLength)
              (fromMaybe (ecEpochSize defConfig) epochSize)
              (fromMaybe (ecMaxTxSize defConfig) maxTxSize)
              (fromMaybe (ecRaiseExUnitsToMax defConfig) raiseExUnitsToMax)

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  maybeClusterStatus <- liftIO $ tryTakeMVar statusMVar
  case maybeClusterStatus of
    Nothing -> pure $ StopClusterFailure "Cluster is not running"
    Just statusTVar -> do
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess
