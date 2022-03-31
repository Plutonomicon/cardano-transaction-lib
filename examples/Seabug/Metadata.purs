module Seabug.Metadata where

import Contract.Prelude

import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Monad (Contract)
import Contract.Prim.ByteArray
import Contract.Transaction
  ( ClientError(ClientHttpError, ClientDecodeJsonError)
  )
import Contract.Value
import Control.Alternative (guard)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Json
import Data.Bifunctor (bimap, lmap)
import Data.Function (on)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)
import Metadata.Seabug (SeabugMetadata(SeabugMetadata))

type Hash = String

getMintingTxSeabugMetadata
  :: Hash -> ExceptT ClientError Contract SeabugMetadata
getMintingTxSeabugMetadata txHash = do
  j <- mkGetRequest $ "txs/" <> txHash <> "/metadata"
  ms <- except
    $ lmap ClientDecodeJsonError
    $ Json.caseJsonArray
        (Left (Json.TypeMismatch "Expected array of objects"))
        Right
        j
  except
    $ note (ClientDecodeJsonError (Json.UnexpectedValue j))
    $ findSeabugMetadata ms
  where
  findSeabugMetadata :: Array Json.Json -> Maybe SeabugMetadata
  findSeabugMetadata = findMap $ Json.caseJsonObject Nothing $ \o -> do
    label <- hush $ Json.getField o "label"
    guard $ label == "727"
    hush $ Json.decodeJson =<< Json.getField o "json_metadata"

getMintingTxHash
  :: CurrencySymbol /\ TokenName -> ExceptT ClientError Contract Hash
getMintingTxHash (currSym /\ tname) =
  except <<< decodeField "initial_mint_tx_hash"
    =<< mkGetRequest ("assets/" <> asset)
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

mkGetRequest :: String -> ExceptT ClientError Contract Json.Json
mkGetRequest path = do
  projectId <- lift $ asks $ _.projectId <<< unwrap
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
  ExceptT $ liftAff $ Affjax.request req <#> bimap ClientHttpError _.body
  where
  mkUrl :: String
  mkUrl = "https://cardano-testnet.blockfrost.io/api/v0/" <> path
