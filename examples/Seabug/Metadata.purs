module Seabug.Metadata where

import Contract.Prelude

import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Monad (Contract)
import Contract.Transaction (ClientError(ClientHttpError))
import Control.Monad.Reader.Trans (asks)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Json
import Data.Bifunctor (bimap)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)

mkGetRequest :: String -> Contract (Either ClientError Json.Json)
mkGetRequest path = do
  projectId <- asks $ _.projectId <<< unwrap
  let
    req :: Affjax.Request Json.Json
    req = Affjax.defaultRequest
      { url = mkUrl
      , responseFormat = Affjax.ResponseFormat.json
      , method = Left GET
      , headers =
          [ Affjax.RequestHeader.RequestHeader "project_id" projectId
          ]
      }
  liftAff $ Affjax.request req <#> bimap ClientHttpError _.body
  where
  mkUrl :: String
  mkUrl = "https://cardano-testnet.blockfrost.io/api/v0/" <> path
