module Plutip.Types where

import Data.Tuple.Nested
import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(..)
  , decodeAeson
  , toStringifiedNumbersJson
  , (.:)
  )
import Data.BigInt (BigInt)
import Data.Either (Either(..))
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
  }

type PrivateKey = String

type FilePath = String

type ErrorMessage = String

type KeyId = Int

type UtxoAmount = BigInt

type InitialUTXOs = Array (KeyId /\ Array UtxoAmount)

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTXOs }

derive newtype instance EncodeAeson ClusterStartupRequest

type ClusterStartupParameters =
  { privateKeys :: Array (Int /\ PrivateKey {- Cbor -} )
  , nodeSocketPath :: FilePath
  , nodeConfigPath :: FilePath
  , keysDirectory :: FilePath
  }

data ClusterStartupFailureReason
  = ClusterIsRunningAlready
  | NegativeLovelaces { keyId :: Int }
  | NodeConfigNotFound

derive instance Generic ClusterStartupFailureReason _

instance Show ClusterStartupFailureReason where
  show = genericShow

instance DecodeAeson ClusterStartupFailureReason where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    tag <- obj .: "tag"
    case tag of
      "ClusterIsRunningAlready" -> do
        pure ClusterIsRunningAlready
      "NegativeLovelaces" -> NegativeLovelaces <$> decodeAeson aeson
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
        ClusterStartupFailure <$> decodeAeson aeson
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))

-- {"nodeSocketPath":"/tmp/nix-shell.BOlzhm/test-cluster921708/node/node.socket","keysDirectory":"/tmp/nix-shell.BOlzhm/test-cluster921708/bot-plutus-interface/signing-keys","tag":"ClusterStartupSuccess","privateKeys":[[1,"58202968654b73b625a598addc6cc7894fcf31e7e00f99e9e87d7788bb362fcb565a"]]}%

data StopClusterResponse = StopClusterSuccess | StopClusterFailure ErrorMessage
