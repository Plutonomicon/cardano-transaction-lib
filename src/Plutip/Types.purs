module Plutip.Types where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , decodeAeson
  , encodeAeson'
  , toStringifiedNumbersJson
  , (.:)
  )
import Data.BigInt (BigInt)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import QueryM.ServerConfig (ServerConfig)

type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  , distribution :: InitialUTXOs
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , ogmiosDatumCacheConfig :: ServerConfig
  , ctlServerConfig :: ServerConfig
  -- Should be synchronized with `defaultConfig.postgres` in `flake.nix`
  , postgresConfig :: PostgresConfig
  }

type PostgresConfig =
  { host :: String
  , port :: UInt
  , user :: String
  , password :: String
  , dbname :: String
  }

type PrivateKey = String

type FilePath = String

type ErrorMessage = String

type KeyId = Int

type UtxoAmount = BigInt

type InitialUTXOs = Array (Array UtxoAmount)

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTXOs }

derive newtype instance EncodeAeson ClusterStartupRequest

type ClusterStartupParameters =
  { privateKeys :: Array PrivateKey
  , nodeSocketPath :: FilePath
  , nodeConfigPath :: FilePath
  , keysDirectory :: FilePath
  }

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces
  | NodeConfigNotFound

derive instance Generic ClusterStartupFailureReason _

instance Show ClusterStartupFailureReason where
  show = genericShow

instance DecodeAeson ClusterStartupFailureReason where
  decodeAeson aeson = do
    tag <- decodeAeson aeson
    case tag of
      "ClusterIsRunningAlready" -> do
        pure ClusterIsRunningAlready
      "NegativeLovelaces" -> pure NegativeLovelaces
      "NodeConfigNotFound" -> pure NodeConfigNotFound
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

data StartClusterResponse
  = ClusterStartupFailure ClusterStartupFailureReason
  | ClusterStartupSuccess ClusterStartupParameters

derive instance Generic StartClusterResponse _

instance Show StartClusterResponse where
  show = genericShow

instance DecodeAeson StartClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    tag <- obj .: "tag"
    case tag of
      "ClusterStartupSuccess" -> do
        ClusterStartupSuccess <$> decodeAeson aeson
      "ClusterStartupFailure" -> do
        failure <- obj .: "contents"
        ClusterStartupFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

data StopClusterRequest = StopClusterRequest

derive instance Generic StopClusterRequest _

instance Show StopClusterRequest where
  show = genericShow

instance EncodeAeson StopClusterRequest where
  encodeAeson' _ = encodeAeson' ([] :: Array Int)

data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage

derive instance Generic StopClusterResponse _

instance Show StopClusterResponse where
  show = genericShow

instance DecodeAeson StopClusterResponse where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    tag <- obj .: "tag"
    case tag of
      "StopClusterSuccess" -> pure StopClusterSuccess
      "StopClusterFailure" -> do
        failure <- obj .: "contents"
        StopClusterFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))
