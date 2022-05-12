{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ogmios.Query (
  queryCurrentProtocolParameters,
) where

--------------------------------------------------------------------------------
import Control.Exception.Base (Exception, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp, ConnectionException)
import Network.WebSockets qualified as WebSockets

import Debug.Trace (trace)

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

mkApp :: RequestOption -> ClientApp Text
mkApp option conn = do
  let requestJSON = buildRequestJSON option
  WebSockets.sendTextData
    conn
    requestJSON
  msg <- WebSockets.receiveData conn
  WebSockets.sendClose conn ("" :: Text)
  return msg

makeRequest :: ServerParameters -> RequestOption -> IO Text
makeRequest ServerParameters {..} option =
  withSocketsDo $
    WebSockets.runClient getHost getPort getPath $
      mkApp option

queryCurrentProtocolParameters :: IO Text
queryCurrentProtocolParameters =
  makeRequest defaultServerParameters $ Query CurrentProtocolParameters

test :: IO Text -> IO ()
test query = do
  eitherMsg <- (try query :: IO (Either ConnectionException Text))
  case eitherMsg of
    Right msg -> print msg
    Left _ -> print (Text.unpack "Error connecting to Ogmios")
