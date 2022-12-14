module Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ErrorMessage
  , FilePath
  , InitialUTxOs
  , InitialUTxODistribution
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , PlutipConfig
  , PostgresConfig
  , ClusterStartupRequest(ClusterStartupRequest)
  , PrivateKeyResponse(PrivateKeyResponse)
  , ClusterStartupFailureReason
      ( ClusterIsRunningAlready
      , NegativeLovelaces
      , NodeConfigNotFound
      )
  , StartClusterResponse
      ( ClusterStartupFailure
      , ClusterStartupSuccess
      )
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse(StopClusterSuccess, StopClusterFailure)
  , UtxoAmount
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , decodeAeson
  , encodeAeson'
  , toStringifiedNumbersJson
  , (.:)
  )
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.QueryM (Hooks)
import Ctl.Internal.QueryM.ServerConfig (ServerConfig)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Ctl.Internal.Wallet.Key (PrivateStakeKey)
import Data.BigInt (BigInt)
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UInt (UInt)
import Effect.Aff (Aff)

type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , ogmiosDatumCacheConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  -- Should be synchronized with `defaultConfig.postgres` in `flake.nix`
  , postgresConfig :: PostgresConfig
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  }

type PostgresConfig =
  { host :: String
  , port :: UInt
  , user :: String
  , password :: String
  , dbname :: String
  }

type FilePath = String

type ErrorMessage = String

-- | UTxO amount in Lovelaces
type UtxoAmount = BigInt

type InitialUTxOs = Array UtxoAmount

data InitialUTxOsWithStakeKey =
  InitialUTxOsWithStakeKey PrivateStakeKey InitialUTxOs

type InitialUTxODistribution = Array InitialUTxOs

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTxODistribution }

derive newtype instance EncodeAeson ClusterStartupRequest

newtype PrivateKeyResponse = PrivateKeyResponse PrivateKey

derive instance Newtype PrivateKeyResponse _
derive instance Generic PrivateKeyResponse _

instance Show PrivateKeyResponse where
  show _ = "(PrivateKeyResponse \"<private key>\")"

instance DecodeAeson PrivateKeyResponse where
  decodeAeson json = do
    cborStr <- decodeAeson json
    let splitted = String.splitAt 4 cborStr
    -- 5820 prefix comes from Cbor
    if splitted.before == "5820" then do
      cborBytes <- note err $ hexToByteArray splitted.after
      PrivateKeyResponse <$> note err (privateKeyFromBytes (RawBytes cborBytes))
    else Left err
    where
    err :: JsonDecodeError
    err = TypeMismatch "PrivateKey"

type ClusterStartupParameters =
  { privateKeys :: Array PrivateKeyResponse
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
    decodeAeson aeson >>= case _ of
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
    obj .: "tag" >>= case _ of
      "ClusterStartupSuccess" -> do
        contents <- obj .: "contents"
        ClusterStartupSuccess <$> decodeAeson contents
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
    obj .: "tag" >>= case _ of
      "StopClusterSuccess" -> pure StopClusterSuccess
      "StopClusterFailure" -> do
        failure <- obj .: "contents"
        StopClusterFailure <$> decodeAeson failure
      _ -> do
        Left (UnexpectedValue (toStringifiedNumbersJson aeson))
