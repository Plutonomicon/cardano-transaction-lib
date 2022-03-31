module Seabug.Metadata where

import Contract.Prelude

import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Monad (Contract)
import Contract.Prim.ByteArray
import Contract.Transaction (ClientError(ClientHttpError, ClientDecodeJsonError))
import Contract.Value
import Control.Monad.Reader.Trans (asks)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Json
import Data.Bifunctor (bimap, lmap)
import Data.Function (on)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)

getMintingTxHash
  :: CurrencySymbol /\ TokenName -> Contract (Either ClientError String)
getMintingTxHash (currSym /\ tname) = mkGetRequest ("assets/" <> asset)
  <#> either Left (decodeField "initial_mint_tx_hash")
  where
  asset :: String
  asset = mkAsset (getCurrencySymbol currSym) (getTokenName tname)

  mkAsset :: ByteArray -> ByteArray -> String
  mkAsset = (<>) `on` byteArrayToHex

decodeField
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> Json.Json
  -> Either ClientError a
decodeField field = lmap ClientDecodeJsonError <<<
  ( Json.decodeJson
      <=< Json.caseJsonObject
        (Left (Json.TypeMismatch "Expected Object"))
        (flip Json.getField field)
  )

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
