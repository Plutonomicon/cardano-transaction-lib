{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ogmios.Query (
  makeRequest,
  tryQueryUntilZero,
  defaultServerParameters,
  ServerParameters (..),
) where

import Control.Exception.Base (SomeException, try)

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.WebSockets (ClientApp)
import Network.WebSockets qualified as WebSockets
import System.IO (hFlush, stdout)
import System.Time.Extra qualified as Time.Extra

--------------------------------------------------------------------------------

data ServerParameters = ServerParameters
  { host :: String
  , -- | WebSockets needs the port as Int.
    port :: Int
  , path :: String
  }

defaultServerParameters :: ServerParameters
defaultServerParameters =
  ServerParameters
    { host = "ogmios"
    , port = 1337
    , path = "/"
    }

requestJSON :: Text
requestJSON =
  "{ \"type\": \"jsonwsp/request\", \"version\": \"1.0\", \"servicename\": \"ogmios\","
    <> "\"methodname\": \"Query\", \"args\": { \"query\":\""
    <> "currentProtocolParameters"
    <> "\"} }"

app :: ClientApp ByteString
app conn = do
  WebSockets.sendTextData
    conn
    requestJSON
  msg <- WebSockets.receiveData conn
  WebSockets.sendClose conn ("" :: Text)
  pure msg

makeRequest :: ServerParameters -> IO ByteString
makeRequest ServerParameters {..} =
  WebSockets.runClient host port path app

tryQueryUntilZero :: IO ByteString -> Int -> IO (Either String ByteString)
tryQueryUntilZero query remainAttempts =
  if remainAttempts <= 0
    then do
      pure $ Left "Error trying to connect to Ogmios"
    else do
      msgOrError <- first show <$> try @SomeException query
      case msgOrError of
        Right msg -> pure $ Right msg
        Left e ->
          do
            putStrLn $ "Error : " <> show e
            putStrLn "Waiting for ogmios conection attempt"
            putStrLn $ "Attempts remain : " <> show (remainAttempts -1)
            Time.Extra.sleep 0.5
            hFlush stdout
            tryQueryUntilZero query (remainAttempts - 1)
