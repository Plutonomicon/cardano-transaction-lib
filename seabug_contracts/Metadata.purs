module Seabug.Metadata
  ( FullSeabugMetadata
  , Hash
  , getFullSeabugMetadata
  ) where

import Contract.Prelude

import Affjax as Affjax
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction
  ( ClientError(ClientHttpError, ClientDecodeJsonError)
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  )
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

type FullSeabugMetadata =
  { seabugMetadata :: SeabugMetadata
  , ipfsHash :: Hash
  }

getFullSeabugMetadata
  :: CurrencySymbol /\ TokenName
  -> Contract (Either ClientError FullSeabugMetadata)
getFullSeabugMetadata a@(currSym /\ _) = runExceptT $ do
  seabugMetadata <- getMintingTxSeabugMetadata currSym =<< getMintingTxHash a
  ipfsHash <- getIpfsHash seabugMetadata
  pure { seabugMetadata, ipfsHash }

getIpfsHash :: SeabugMetadata -> ExceptT ClientError Contract Hash
getIpfsHash (SeabugMetadata { collectionNftCS, collectionNftTN }) = do
  except <<< (decodeField "image" <=< decodeField "onchain_metadata")
    =<< mkGetRequest ("assets/" <> mkAsset collectionNftCS collectionNftTN)

getMintingTxSeabugMetadata
  :: CurrencySymbol -> Hash -> ExceptT ClientError Contract SeabugMetadata
getMintingTxSeabugMetadata currSym txHash = do
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
    hush $ do
      md <- Json.getField o "json_metadata"
      Json.decodeJson =<< Json.getField md currSymKey

  currSymKey :: String
  currSymKey = byteArrayToHex $ getCurrencySymbol currSym

getMintingTxHash
  :: CurrencySymbol /\ TokenName -> ExceptT ClientError Contract Hash
getMintingTxHash a =
  except <<< decodeField "initial_mint_tx_hash"
    =<< mkGetRequest ("assets/" <> uncurry mkAsset a)

mkAsset :: CurrencySymbol -> TokenName -> String
mkAsset currSym tname =
  ((<>) `on` byteArrayToHex) (getCurrencySymbol currSym) (getTokenName tname)

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
