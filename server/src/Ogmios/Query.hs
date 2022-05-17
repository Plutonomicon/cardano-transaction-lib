{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ogmios.Query (
  makeRequest,
  tryQueryUntilZero,
  queryCurrentProtocolParameters,
) where

--------------------------------------------------------------------------------

import Control.Exception (IOException)
import Control.Exception.Base (try)

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Socket (withSocketsDo)
import Network.WebSockets (ClientApp)
import Network.WebSockets qualified as WebSockets
import System.IO (hFlush, stdout)
import System.Time.Extra qualified as Time.Extra

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
    { getHost = "ogmios"
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

makeApp :: RequestOption -> ClientApp ByteString
makeApp option conn = do
  let requestJSON = buildRequestJSON option
  print ("Sending request : " <> requestJSON)
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
      makeApp option

queryCurrentProtocolParameters :: IO ByteString
queryCurrentProtocolParameters =
  makeRequest defaultServerParameters $ Query CurrentProtocolParameters

tryQueryUntilZero :: IO ByteString -> Int -> IO (Either String ByteString)
tryQueryUntilZero query remainAttempts =
  if remainAttempts <= 0
    then do
      print "Error trying to connect to Ogmios"
      hFlush stdout
      return $ Left "Error trying to connect to Ogmios"
    else do
      msgOrError <- (try query :: IO (Either IOException ByteString))
      case msgOrError of
        Right msg -> return $ Right msg
        Left e ->
          do
            putStrLn $ "Error : " <> show e
            putStrLn "Waiting for ogmios conection attempt"
            putStrLn $ "Attempts remain : " <> show (remainAttempts -1)
            Time.Extra.sleep 0.5
            hFlush stdout
            tryQueryUntilZero query (remainAttempts - 1)
