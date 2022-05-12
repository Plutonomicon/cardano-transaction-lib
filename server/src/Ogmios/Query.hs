{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ogmios.Query (
  makeRequest,
  tryQueryUntilZero,
  queryCurrentProtocolParameters,
) where

--------------------------------------------------------------------------------

import Control.Exception (IOException, SomeException (SomeException))
import Control.Exception.Base (Exception, catch, try)
import Data.Typeable (tyConModule, tyConPackage, typeOf, typeRepTyCon)

import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp, ConnectionException)
import Network.WebSockets qualified as WebSockets

import Cardano.Api (FromJSON)
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Api.Shelley qualified as Shelley
import Servant (ErrorFormatters (headerParseErrorFormatter))

--------------------------------------------------------------------------------

data QueryOption = CurrentProtocolParameters

data RequestOption
  = Query QueryOption
  | -- | TODO : add point parameter to Acquire
    Acquire

data ServerParameters = ServerParameters
  { getHost :: String
  , -- | WebSockets needs the port as Int.
    getPort :: Int
  , getPath :: String
  }

newtype ProtocolParametersWrapper
  = ProtocolParametersWrapper Shelley.ProtocolParameters

instance FromJSON ProtocolParametersWrapper where
  parseJSON =
    Aeson.withObject "ProtocolParametersWrapper" $ \top -> do
      o :: Aeson.Object <- top .: "result"
      v <- o .: "protocolVersion"
      params <-
        Shelley.ProtocolParameters
          <$> ((,) <$> v .: "major" <*> v .: "minor")
          <*> o .: "decentralizationParameter"
          <*> o .: "extraEntropy"
          <*> o .: "maxBlockHeaderSize"
          <*> o .: "maxBlockBodySize"
          <*> o .: "maxTxSize"
          <*> o .: "minFeeConstant" -- I think minFeeConstant and minFeeCoefficient are swapped here
          -- but this is consistent with the current format.
          <*> o .: "minFeeCoefficient"
          <*> o .: "minUTxOValue" -- Don't what's the corresponding value
          <*> o .: "stakeKeyDeposit"
          <*> o .: "poolDeposit"
          <*> o .: "minPoolCost"
          <*> o .: "poolRetirementEpochBound"
          <*> o .: "stakePoolTargetNum" -- COntinue Here
          <*> o .: "poolPledgeInfluence"
          <*> o .: "monetaryExpansion"
          <*> o .: "treasuryCut"
          <*> o .:? "utxoCostPerWord"
          <*> o .:? "costModels" .!= Map.empty
          <*> o .:? "executionUnitPrices"
          <*> o .:? "maxTxExecutionUnits"
          <*> o .:? "maxBlockExecutionUnits"
          <*> o .:? "maxValueSize"
          <*> o .:? "collateralPercentage"
          <*> o .:? "maxCollateralInputs"
      return $ ProtocolParametersWrapper params

defaultServerParameters :: ServerParameters
defaultServerParameters =
  ServerParameters
    { getHost = "localhost"
    , getPort = 1337
    , getPath = "/"
    }

getQueryName :: QueryOption -> Text
getQueryName CurrentProtocolParameters = "currentProtocolParameters"

buildRequestJSON :: RequestOption -> Text
buildRequestJSON (Query CurrentProtocolParameters) =
  "{ \"type\": \"jsonwsp/request\", \"version\": \"1.0\", \"servicename\": \"ogmios\","
    <> "\"methodname\": \"Query\", \"args\": { \"query\":\""
    <> getQueryName CurrentProtocolParameters
    <> "\"} }"
buildRequestJSON _ = error "Unsuported Ogmios request"

mkApp :: RequestOption -> ClientApp ByteString
mkApp option conn = do
  let requestJSON = buildRequestJSON option
  WebSockets.sendTextData
    conn
    requestJSON
  msg <- WebSockets.receiveData conn
  WebSockets.sendClose conn ("" :: Text)
  return msg

makeRequest :: ServerParameters -> RequestOption -> IO ByteString
makeRequest ServerParameters {..} option =
  withSocketsDo $
    WebSockets.runClient getHost getPort getPath $
      mkApp option

queryCurrentProtocolParameters :: IO ByteString
queryCurrentProtocolParameters =
  makeRequest defaultServerParameters $ Query CurrentProtocolParameters

tryQueryUntilZero :: IO ByteString -> Int -> IO (Either Text ByteString)
tryQueryUntilZero query remainAttempts =
  if remainAttempts <= 0
    then return $ Left "Error connection to Ogmios"
    else do
      msgOrError <- (try query :: IO (Either IOException ByteString))
      case msgOrError of
        Right msg -> return $ Right msg
        _ -> tryQueryUntilZero query (remainAttempts - 1)

test :: IO ByteString -> IO ()
test query = do
  eitherMsg <- (try query :: IO (Either IOException ByteString))
  case eitherMsg of
    Right msg ->
      ( case (Aeson.decode msg :: Maybe Aeson.Value) of
          Just (Aeson.Object hashmap) -> do
            print hashmap
            putStrLn ""
            print msg
          _ -> print (Text.unpack "Error, wrong format")
      )
    _ -> print (Text.unpack "Error connecting to Ogmios")
