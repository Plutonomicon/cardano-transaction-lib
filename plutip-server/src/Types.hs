module Types (
  AppM (AppM),
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
    NegativeLovelaces,
    NodeConfigNotFound
  ),
  Env (Env, status, options),
  ErrorMessage,
  Lovelace (unLovelace),
  PrivateKey,
  ServerOptions (ServerOptions, port),
  StartClusterRequest (
    StartClusterRequest,
    keysToGenerate,
    slotLength,
    epochSize,
    maxTxSize,
    raiseExUnitsToMax
  ),
  StartClusterResponse (
    ClusterStartupSuccess,
    ClusterStartupFailure
  ),
  ClusterStartupParameters (
    ClusterStartupParameters,
    keysDirectory,
    nodeSocketPath,
    privateKeys,
    nodeConfigPath
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterSuccess, StopClusterFailure),
) where

import Cardano.Ledger.Slot (EpochSize)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Numeric.Natural (Natural)
import Plutip.Config (NominalDiffTimeMicro)
import Plutip.Cluster (StopClusterRef)

-- TVar is used for signaling by 'startCluster'/'stopCluster' (STM is used
-- for blocking).
-- MVar is used by plutip-server to store current TVar (we allow maximum of one
-- cluster at any given moment).
-- This MVar is used by start/stop handlers.
-- The payload of ClusterStatus is irrelevant.
type ClusterStatusRef = MVar StopClusterRef

data Env = Env
  { status :: ClusterStatusRef
  , options :: ServerOptions
  }

data ServerOptions = ServerOptions
  { port :: Port
  }
  deriving stock (Generic)

newtype AppM (a :: Type) = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

type ErrorMessage = Text

newtype Lovelace = Lovelace {unLovelace :: Integer}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, Num, Enum, Ord, Real, Integral)

instance FromJSON Lovelace where
  parseJSON json = do
    Lovelace <$> parseJSON json

data StartClusterRequest = StartClusterRequest
  { keysToGenerate :: [[Lovelace]]
  -- ^ Lovelace amounts for each UTXO of each wallet
  , slotLength :: Maybe NominalDiffTimeMicro
  -- ^ Set the SlotLength. If set to Nothing use the default
  , epochSize :: Maybe EpochSize
  -- ^ Set the EpochSize. If set to Nothing use the default
  , maxTxSize :: Maybe Natural
  -- ^ Set The maxTxSize. If set to Nothing use the default
  , raiseExUnitsToMax :: Maybe Bool
  -- ^ Raise the execution units to the maximum when true.
  -- If set to Nothing use the default
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- CborHex
type PrivateKey = Text

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces
  | NodeConfigNotFound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClusterStartupParameters = ClusterStartupParameters
  { privateKeys :: [PrivateKey]
  , nodeSocketPath :: FilePath
  , nodeConfigPath :: FilePath
  , keysDirectory :: FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StartClusterResponse
  = ClusterStartupFailure ClusterStartupFailureReason
  | ClusterStartupSuccess ClusterStartupParameters
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StopClusterRequest = StopClusterRequest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
