module Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ErrorMessage
  , FilePath
  , PlutipConfig
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
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , decodeAeson
  , encodeAeson
  , finiteNumber
  , toStringifiedNumbersJson
  , (.:)
  )
import Ctl.Internal.Contract.Hooks (Hooks)
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.ServerConfig (ServerConfig)
import Ctl.Internal.Test.UtxoDistribution (InitialUTxODistribution)
import Ctl.Internal.Types.ByteArray (hexToByteArray)
import Ctl.Internal.Types.RawBytes (RawBytes(RawBytes))
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)

-- | A config that is used to run tests on Plutip clusters.
-- | Note that the test suite starts the services on the specified ports.
-- | It does not expect them to be running.
type PlutipConfig =
  { host :: String
  , port :: UInt
  , logLevel :: LogLevel
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  , hooks :: Hooks
  , clusterConfig ::
      { slotLength :: Seconds
      , epochSize :: Maybe UInt
      , maxTxSize :: Maybe UInt
      , raiseExUnitsToMax :: Boolean
      }
  }

type FilePath = String

type ErrorMessage = String

newtype ClusterStartupRequest = ClusterStartupRequest
  { keysToGenerate :: InitialUTxODistribution
  , epochSize :: UInt
  , slotLength :: Seconds
  , maxTxSize :: Maybe UInt
  , raiseExUnitsToMax :: Boolean
  }

instance EncodeAeson ClusterStartupRequest where
  encodeAeson
    ( ClusterStartupRequest
        { keysToGenerate
        , epochSize
        , slotLength: Seconds slotLength
        , maxTxSize
        , raiseExUnitsToMax
        }
    ) = encodeAeson
    { keysToGenerate
    , epochSize
    , slotLength: unsafePartial $ fromJust $ finiteNumber slotLength
    , maxTxSize
    , raiseExUnitsToMax
    }

newtype PrivateKeyResponse = PrivateKeyResponse PrivateKey

derive instance Newtype PrivateKeyResponse _
derive instance Generic PrivateKeyResponse _

instance Show PrivateKeyResponse where
  show _ = "(PrivateKeyResponse \"<private key>\")"

instance DecodeAeson PrivateKeyResponse where
  decodeAeson json = do
    cborStr <- decodeAeson json
    cborBytes <- note err $ hexToByteArray cborStr
    PrivateKeyResponse <$> note err (privateKeyFromBytes (RawBytes cborBytes))
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
  encodeAeson _ = encodeAeson ([] :: Array Int)

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
