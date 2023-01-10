module Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint
      ( Transaction
      , TransactionMetadata
      )
  , BlockfrostMetadata(BlockfrostMetadata)
  , BlockfrostRawResponses
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , getTxMetadata
  , isTxConfirmed
  , runBlockfrostServiceM
  , runBlockfrostServiceTestM
  , dummyExport
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , parseJsonStringToAeson
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataClientError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.Transaction
  ( convertGeneralTransactionMetadata
  )
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (modify_, new, read) as Ref
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- BlockfrostServiceM
--------------------------------------------------------------------------------

type BlockfrostRawResponses = Maybe (Ref (Map BlockfrostEndpoint String))

type BlockfrostServiceParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , blockfrostRawResponses :: BlockfrostRawResponses
  }

type BlockfrostServiceM (a :: Type) = ReaderT BlockfrostServiceParams Aff a

runBlockfrostServiceM
  :: forall (a :: Type). BlockfrostBackend -> BlockfrostServiceM a -> Aff a
runBlockfrostServiceM = flip runReaderT <<< mkServiceParams Nothing

runBlockfrostServiceTestM
  :: forall (a :: Type)
   . BlockfrostBackend
  -> BlockfrostServiceM a
  -> Aff (a /\ Map BlockfrostEndpoint String)
runBlockfrostServiceTestM backend action = do
  rawResponsesRef <- liftEffect $ Just <$> Ref.new Map.empty
  runReaderT actionWithRawResponses (mkServiceParams rawResponsesRef backend)
  where
  actionWithRawResponses
    :: BlockfrostServiceM (a /\ Map BlockfrostEndpoint String)
  actionWithRawResponses = do
    rawResponsesRef <- unsafePartial fromJust <$> asks _.blockfrostRawResponses
    Tuple <$> action <*> liftEffect (Ref.read rawResponsesRef)

mkServiceParams
  :: BlockfrostRawResponses -> BlockfrostBackend -> BlockfrostServiceParams
mkServiceParams blockfrostRawResponses backend =
  { blockfrostConfig: backend.blockfrostConfig
  , blockfrostApiKey: backend.blockfrostApiKey
  , blockfrostRawResponses
  }

--------------------------------------------------------------------------------
-- Making requests to Blockfrost endpoints
--------------------------------------------------------------------------------

data BlockfrostEndpoint
  = Transaction TransactionHash
  | TransactionMetadata TransactionHash

derive instance Generic BlockfrostEndpoint _
derive instance Eq BlockfrostEndpoint
derive instance Ord BlockfrostEndpoint

instance Show BlockfrostEndpoint where
  show = genericShow

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    Transaction txHash -> "/txs/" <> byteArrayToHex (unwrap txHash)
    TransactionMetadata txHash -> "/txs/" <> byteArrayToHex (unwrap txHash)
      <> "/metadata/cbor"

dummyExport :: Unit -> Unit
dummyExport _ = undefined blockfrostPostRequest

blockfrostGetRequest
  :: BlockfrostEndpoint
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostGetRequest endpoint = ask >>= \params ->
  updateRawResponses endpoint =<< liftAff do
    Affjax.request $ Affjax.defaultRequest
      { method = Left GET
      , url = mkHttpUrl params.blockfrostConfig <> realizeEndpoint endpoint
      , responseFormat = Affjax.ResponseFormat.string
      , headers =
          maybe mempty (\apiKey -> [ Affjax.RequestHeader "project_id" apiKey ])
            params.blockfrostApiKey
      }

blockfrostPostRequest
  :: BlockfrostEndpoint
  -> MediaType
  -> Maybe Affjax.RequestBody
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostPostRequest endpoint mediaType mbContent = ask >>= \params ->
  updateRawResponses endpoint =<< liftAff do
    Affjax.request $ Affjax.defaultRequest
      { method = Left POST
      , url = mkHttpUrl params.blockfrostConfig <> realizeEndpoint endpoint
      , content = mbContent
      , responseFormat = Affjax.ResponseFormat.string
      , headers =
          [ Affjax.ContentType mediaType ] <>
            maybe mempty
              (\apiKey -> [ Affjax.RequestHeader "project_id" apiKey ])
              params.blockfrostApiKey
      }

updateRawResponses
  :: BlockfrostEndpoint
  -> Either Affjax.Error (Affjax.Response String)
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
updateRawResponses endpoint result@(Right { body: response }) = do
  rawResponses <- asks _.blockfrostRawResponses
  liftEffect $
    traverse_ (Ref.modify_ (Map.insert endpoint response)) rawResponses
  pure result
updateRawResponses _ result = pure result

--------------------------------------------------------------------------------
-- Blockfrost response handling
--------------------------------------------------------------------------------

handleBlockfrostResponse
  :: forall (result :: Type)
   . DecodeAeson result
  => Either Affjax.Error (Affjax.Response String)
  -> Either ClientError result
handleBlockfrostResponse (Left affjaxError) =
  Left (ClientHttpError affjaxError)
handleBlockfrostResponse (Right { status: Affjax.StatusCode statusCode, body })
  | statusCode < 200 || statusCode > 299 = do
      blockfrostError <-
        body # lmap (ClientDecodeJsonError body)
          <<< (decodeAeson <=< parseJsonStringToAeson)
      Left $ ClientHttpResponseError (wrap statusCode) $
        ServiceBlockfrostError blockfrostError
  | otherwise =
      body # lmap (ClientDecodeJsonError body)
        <<< (decodeAeson <=< parseJsonStringToAeson)

isTxConfirmed
  :: TransactionHash
  -> BlockfrostServiceM (Either ClientError Boolean)
isTxConfirmed txHash = do
  response <- blockfrostGetRequest $ Transaction txHash
  pure case handleBlockfrostResponse response of
    Right (_ :: Aeson) -> Right true
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) -> Right false
    Left e -> Left e

getTxMetadata
  :: TransactionHash
  -> BlockfrostServiceM (Either GetTxMetadataError GeneralTransactionMetadata)
getTxMetadata txHash = do
  response <- blockfrostGetRequest (TransactionMetadata txHash)
  pure case unwrapBlockfrostMetadata <$> handleBlockfrostResponse response of
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) ->
      Left GetTxMetadataTxNotFoundError
    Left e ->
      Left (GetTxMetadataClientError e)
    Right metadata
      | Map.isEmpty (unwrap metadata) ->
          Left GetTxMetadataMetadataEmptyOrMissingError
      | otherwise -> Right metadata

--------------------------------------------------------------------------------
-- `getTxMetadata` reponse parsing
--------------------------------------------------------------------------------

newtype BlockfrostMetadata = BlockfrostMetadata
  GeneralTransactionMetadata

derive instance Generic BlockfrostMetadata _
derive instance Eq BlockfrostMetadata
derive instance Newtype BlockfrostMetadata _

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
