module Ctl.Internal.Service.Blockfrost
  ( isTxConfirmed
  , getTxMetadata
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestHeader (RequestHeader(RequestHeader)) as Affjax
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.Transaction
  ( convertGeneralTransactionMetadata
  )
import Ctl.Internal.QueryM
  ( ClientError(ClientHttpResponseError)
  , handleAffjaxResponse
  )
import Ctl.Internal.QueryM.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Effect.Aff (Aff)

isTxConfirmed
  :: TransactionHash
  -> ServerConfig
  -> Maybe String
  -> Aff (Either ClientError Boolean)
isTxConfirmed txHash config mbApiKey = do
  response :: Either ClientError Aeson <- handleAffjaxResponse <$> request
  pure case response of
    Right _ -> Right true
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) -> Right false
    Left e -> Left e
  where
  request :: Aff (Either Affjax.Error (Affjax.Response String))
  request = Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = mkHttpUrl config <> endpoint
    , responseFormat = Affjax.ResponseFormat.string
    , headers =
        flip foldMap mbApiKey \apiKey ->
          [ Affjax.RequestHeader "project_id" apiKey ]
    }

  endpoint :: Affjax.URL
  endpoint = "/txs/" <> byteArrayToHex (unwrap txHash)

getTxMetadata
  :: TransactionHash
  -> ServerConfig
  -> Maybe String
  -> Aff (Either ClientError (Maybe GeneralTransactionMetadata))
getTxMetadata txHash config mbApiKey = do
  response :: Either ClientError _ <- handleAffjaxResponse <$> request
  pure case response of
    Right metadata -> Right $ Just $ unwrapBlockfrostMetadata metadata
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) -> Right Nothing
    Left e -> Left e
  where
  request :: Aff (Either Affjax.Error (Affjax.Response String))
  request = Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = mkHttpUrl config <> endpoint
    , responseFormat = Affjax.ResponseFormat.string
    , headers =
        flip foldMap mbApiKey \apiKey ->
          [ Affjax.RequestHeader "project_id" apiKey ]
    }

  endpoint :: Affjax.URL
  endpoint = "/txs/" <> byteArrayToHex (unwrap txHash) <> "/metadata/cbor"

--------------------------------------------------------------------------------
-- `getTxMetadata` reponse parsing
--------------------------------------------------------------------------------

newtype BlockfrostMetadata = BlockfrostMetadata
  GeneralTransactionMetadata

derive instance Generic BlockfrostMetadata _
derive instance Eq BlockfrostMetadata

instance Show BlockfrostMetadata where
  show = genericShow

instance DecodeAeson BlockfrostMetadata where
  decodeAeson = decodeAeson >=>
    \(metadatas :: Array { metadata :: CborBytes }) -> do
      metadatas' <- for metadatas \{ metadata } -> do
        map (unwrap <<< convertGeneralTransactionMetadata) <$> flip note
          (fromBytes metadata) $
          TypeMismatch "Hexadecimal encoded Metadata"

      pure $ BlockfrostMetadata $ GeneralTransactionMetadata $ Map.unions
        metadatas'

unwrapBlockfrostMetadata :: BlockfrostMetadata -> GeneralTransactionMetadata
unwrapBlockfrostMetadata (BlockfrostMetadata metadata) = metadata
