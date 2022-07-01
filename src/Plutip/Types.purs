module Plutip.Types where

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
import Data.BigInt (BigInt)
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UInt (UInt)
import QueryM.ServerConfig (ServerConfig)
import Serialization (privateKeyFromBytes)
import Serialization.Types (PrivateKey)
import Types.ByteArray (hexToByteArray)
import Types.RawBytes (RawBytes(RawBytes))

type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  , distribution :: Array InitialUtxos
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

type FilePath = String

type ErrorMessage = String

type KeyId = Int

type UtxoAmount = BigInt

type InitialUTXOs = Array (Array UtxoAmount)

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTXOs }

derive newtype instance EncodeAeson ClusterStartupRequest

newtype PrivateKeyResponse = PrivateKeyResponse PrivateKey

derive instance Newtype PrivateKeyResponse _
derive instance Generic PrivateKeyResponse _

instance Show PrivateKeyResponse where
  show _ = "(PrivateKeyResponse \"<private key>\")"

instance DecodeAeson PrivateKeyResponse where
  decodeAeson json = do
    cborStr :: String <- decodeAeson json
    let splitted = String.splitAt 4 cborStr
    if splitted.before == "5820" then do
      cborBytes <- note err $ hexToByteArray splitted.after
      PrivateKeyResponse <$> note err (privateKeyFromBytes (RawBytes cborBytes))
    else Left err
    where
    err = (TypeMismatch "PrivateKey")

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

type InitialUtxos = Array UtxoAmount

class UtxoDistribution spec wallets | spec -> wallets, wallets -> spec where
  encodeSpec :: spec -> Array (Array UtxoAmount)
  decodeWallets :: Array PrivateKey -> Maybe wallets

-- instance UtxoDistribution InitialUtxos KeyWallet where
--   encodeSpec amounts = [ amounts ]
--   decodeWallets [ x ] = pure x
--   decodeWallets _ = Nothing

-- instance
--   UtxoDistribution restSpec restWallets =>
--   UtxoDistribution (InitialUtxos /\ restSpec) (PrivateKey /\ restWallets) where
--     encodeSpec (amounts /\ rest) = encodeSpec amounts <> encodeSpec rest
--     decodeWallets = Array.uncons >>> case _ of
--       Nothing -> Nothing
--       Just { head, tail } ->
--         Tuple head <$> decodeWallets tail
