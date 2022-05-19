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

import Data.Aeson qualified as Aeson

import GHC.Generics

--------------------------------------------------------------------------------

data ServerParameters = ServerParameters
  { host :: String
  , -- | WebSockets needs the port as Int.
    port :: Int
  , path :: String
  }

newtype Args = Args
  { query :: Text
  }
  deriving stock (Generic, Show)

instance Aeson.ToJSON Args where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Request = Request
  { requestType :: Text
  , version :: Text
  , servicename :: Text
  , methodname :: Text
  , args :: Args
  }
  deriving stock (Generic, Show)

instance Aeson.ToJSON Request where
  toEncoding =
    Aeson.genericToEncoding
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier = \s ->
            if s == "requestType" then "type" else s
        }

defaultRequest :: Request
defaultRequest =
  Request
    { requestType = "jsonwsp/request"
    , version = "1.0"
    , servicename = "ogmios"
    , methodname = "Query"
    , args = Args "currentProtocolParameters"
    }

defaultServerParameters :: ServerParameters
defaultServerParameters =
  ServerParameters
    { host = "ogmios"
    , port = 1337
    , path = "/"
    }

app :: ClientApp ByteString
app conn = do
  WebSockets.sendTextData
    conn
    $ Aeson.encode defaultRequest
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
        Left e -> do
          putStrLn $ "Error : " <> show e
          putStrLn "Waiting for ogmios conection attempt"
          putStrLn $ "Attempts remain : " <> show (remainAttempts -1)
          Time.Extra.sleep 0.5
          hFlush stdout
          tryQueryUntilZero query (remainAttempts - 1)
