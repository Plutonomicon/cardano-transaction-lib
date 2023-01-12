module Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint
      ( DatumCbor
      , NativeScriptByHash
      , PlutusScriptCborByHash
      , ScriptInfo
      , Transaction
      , TransactionMetadata
      )
  , BlockfrostMetadata(BlockfrostMetadata)
  , BlockfrostNativeScript(BlockfrostNativeScript)
  , BlockfrostRawPostResponseData
  , BlockfrostRawResponse
  , BlockfrostScriptInfo(BlockfrostScriptInfo)
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , BlockfrostScriptLanguage(NativeScript, PlutusV1Script, PlutusV2Script)
  , OnBlockfrostRawGetResponseHook
  , OnBlockfrostRawPostResponseHook
  , dummyExport
  , getDatumByHash
  , getScriptByHash
  , getScriptInfo
  , getTxMetadata
  , isTxConfirmed
  , runBlockfrostServiceM
  , runBlockfrostServiceTestM
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , getField
  , getFieldOptional
  , isNull
  , parseJsonStringToAeson
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptAll
      , ScriptAny
      , ScriptNOfK
      , ScriptPubkey
      , TimelockExpiry
      , TimelockStart
      )
  )
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataTxNotFoundError
      , GetTxMetadataClientError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
import Ctl.Internal.Deserialization.Transaction
  ( convertGeneralTransactionMetadata
  )
import Ctl.Internal.Serialization.Hash
  ( ScriptHash
  , ed25519KeyHashFromBytes
  , scriptHashToBytes
  )
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Service.Helpers (aesonObject, aesonString)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum)
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts (plutusV1Script, plutusV2Script)
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Map (isEmpty, unions) as Map
import Data.Maybe (Maybe(Nothing), maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, for_, traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Undefined (undefined)

--------------------------------------------------------------------------------
-- BlockfrostServiceM
--------------------------------------------------------------------------------

type BlockfrostRawResponse = String

type BlockfrostRawPostResponseData =
  { endpoint :: BlockfrostEndpoint
  , mediaType :: MediaType
  , requestBody :: Maybe Affjax.RequestBody
  , rawResponse :: BlockfrostRawResponse
  }

type OnBlockfrostRawGetResponseHook =
  Maybe (BlockfrostEndpoint -> BlockfrostRawResponse -> Aff Unit)

type OnBlockfrostRawPostResponseHook =
  Maybe (BlockfrostRawPostResponseData -> Aff Unit)

type BlockfrostServiceParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  , onBlockfrostRawGetResponse :: OnBlockfrostRawGetResponseHook
  , onBlockfrostRawPostResponse :: OnBlockfrostRawPostResponseHook
  }

type BlockfrostServiceM (a :: Type) = ReaderT BlockfrostServiceParams Aff a

runBlockfrostServiceM
  :: forall (a :: Type). BlockfrostBackend -> BlockfrostServiceM a -> Aff a
runBlockfrostServiceM = flip runReaderT <<< mkServiceParams Nothing Nothing

runBlockfrostServiceTestM
  :: forall (a :: Type)
   . BlockfrostBackend
  -> OnBlockfrostRawGetResponseHook
  -> OnBlockfrostRawPostResponseHook
  -> BlockfrostServiceM a
  -> Aff a
runBlockfrostServiceTestM backend onRawGetResponse onRawPostResponse =
  flip runReaderT (mkServiceParams onRawGetResponse onRawPostResponse backend)

mkServiceParams
  :: OnBlockfrostRawGetResponseHook
  -> OnBlockfrostRawPostResponseHook
  -> BlockfrostBackend
  -> BlockfrostServiceParams
mkServiceParams onBlockfrostRawGetResponse onBlockfrostRawPostResponse backend =
  { blockfrostConfig: backend.blockfrostConfig
  , blockfrostApiKey: backend.blockfrostApiKey
  , onBlockfrostRawGetResponse
  , onBlockfrostRawPostResponse
  }

--------------------------------------------------------------------------------
-- Making requests to Blockfrost endpoints
--------------------------------------------------------------------------------

data BlockfrostEndpoint
  -- /scripts/datum/{datum_hash}/cbor
  = DatumCbor DataHash
  -- /scripts/{script_hash}/json
  | NativeScriptByHash ScriptHash
  -- /scripts/{script_hash}/cbor
  | PlutusScriptCborByHash ScriptHash
  -- /scripts/{script_hash}
  | ScriptInfo ScriptHash
  -- /txs/{hash}
  | Transaction TransactionHash
  -- /txs/{hash}/metadata
  | TransactionMetadata TransactionHash

derive instance Generic BlockfrostEndpoint _
derive instance Eq BlockfrostEndpoint
derive instance Ord BlockfrostEndpoint

instance Show BlockfrostEndpoint where
  show = genericShow

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    DatumCbor (DataHash hashBytes) ->
      "/scripts/datum/" <> byteArrayToHex hashBytes <> "/cbor"
    NativeScriptByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/json"
    PlutusScriptCborByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/cbor"
    ScriptInfo scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash)
    Transaction txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash)
    TransactionMetadata txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash) <> "/metadata/cbor"

dummyExport :: Unit -> Unit
dummyExport _ = undefined blockfrostPostRequest

blockfrostGetRequest
  :: BlockfrostEndpoint
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostGetRequest endpoint = ask >>= \params ->
  withOnRawGetResponseHook endpoint =<< liftAff do
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
  withOnRawPostResponseHook endpoint mediaType mbContent =<< liftAff do
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

withOnRawGetResponseHook
  :: BlockfrostEndpoint
  -> Either Affjax.Error (Affjax.Response String)
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
withOnRawGetResponseHook endpoint result = do
  for_ result \{ body: rawResponse } -> do
    onRawGetResponse <- asks _.onBlockfrostRawGetResponse
    liftAff $ for_ onRawGetResponse \f -> f endpoint rawResponse
  pure result

withOnRawPostResponseHook
  :: BlockfrostEndpoint
  -> MediaType
  -> Maybe Affjax.RequestBody
  -> Either Affjax.Error (Affjax.Response String)
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
withOnRawPostResponseHook endpoint mediaType requestBody result = do
  for_ result \{ body: rawResponse } -> do
    let data_ = { endpoint, mediaType, requestBody, rawResponse }
    onRawPostResponse <- asks _.onBlockfrostRawPostResponse
    liftAff $ for_ onRawPostResponse \f -> f data_
  pure result

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

handle404AsNothing
  :: forall (x :: Type)
   . Either ClientError (Maybe x)
  -> Either ClientError (Maybe x)
handle404AsNothing (Left (ClientHttpResponseError (Affjax.StatusCode 404) _)) =
  Right Nothing
handle404AsNothing x = x

--------------------------------------------------------------------------------
-- Get datum by hash
--------------------------------------------------------------------------------

getDatumByHash
  :: DataHash -> BlockfrostServiceM (Either ClientError (Maybe Datum))
getDatumByHash dataHash = do
  response <- blockfrostGetRequest (DatumCbor dataHash)
  pure $ handle404AsNothing $ unwrapBlockfrostDatum <$> handleBlockfrostResponse
    response

--------------------------------------------------------------------------------
-- Get script by hash
--------------------------------------------------------------------------------

getScriptByHash
  :: ScriptHash
  -> BlockfrostServiceM (Either ClientError (Maybe ScriptRef))
getScriptByHash scriptHash = runExceptT $ runMaybeT do
  scriptInfo <- MaybeT $ ExceptT $ getScriptInfo scriptHash
  case scriptLanguage scriptInfo of
    NativeScript ->
      NativeScriptRef <$>
        (MaybeT $ ExceptT getNativeScriptByHash)
    PlutusV1Script ->
      PlutusScriptRef <$> plutusV1Script <$>
        (MaybeT $ ExceptT getPlutusScriptCborByHash)
    PlutusV2Script ->
      PlutusScriptRef <$> plutusV2Script <$>
        (MaybeT $ ExceptT getPlutusScriptCborByHash)
  where
  getNativeScriptByHash
    :: BlockfrostServiceM (Either ClientError (Maybe NativeScript))
  getNativeScriptByHash = runExceptT do
    (nativeScript :: Maybe BlockfrostNativeScript) <- ExceptT do
      response <- blockfrostGetRequest (NativeScriptByHash scriptHash)
      pure $ handle404AsNothing $ handleBlockfrostResponse response
    pure $ unwrap <$> nativeScript

  getPlutusScriptCborByHash
    :: BlockfrostServiceM (Either ClientError (Maybe ByteArray))
  getPlutusScriptCborByHash = runExceptT do
    (plutusScriptCbor :: Maybe BlockfrostCbor) <- ExceptT do
      response <- blockfrostGetRequest (PlutusScriptCborByHash scriptHash)
      pure $ handle404AsNothing $ handleBlockfrostResponse response
    pure $ join $ unwrap <$> plutusScriptCbor

getScriptInfo
  :: ScriptHash
  -> BlockfrostServiceM (Either ClientError (Maybe BlockfrostScriptInfo))
getScriptInfo scriptHash = do
  response <- blockfrostGetRequest (ScriptInfo scriptHash)
  pure $ handle404AsNothing $ handleBlockfrostResponse response

--------------------------------------------------------------------------------
-- Check transaction confirmation status
--------------------------------------------------------------------------------

isTxConfirmed
  :: TransactionHash
  -> BlockfrostServiceM (Either ClientError Boolean)
isTxConfirmed txHash = do
  response <- blockfrostGetRequest $ Transaction txHash
  pure case handleBlockfrostResponse response of
    Right (_ :: Aeson) -> Right true
    Left (ClientHttpResponseError (Affjax.StatusCode 404) _) -> Right false
    Left e -> Left e

--------------------------------------------------------------------------------
-- Get transaction metadata
--------------------------------------------------------------------------------

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
-- BlockfrostScriptLanguage
--------------------------------------------------------------------------------

data BlockfrostScriptLanguage = NativeScript | PlutusV1Script | PlutusV2Script

derive instance Generic BlockfrostScriptLanguage _
derive instance Eq BlockfrostScriptLanguage

instance Show BlockfrostScriptLanguage where
  show = genericShow

instance DecodeAeson BlockfrostScriptLanguage where
  decodeAeson = aesonString $ case _ of
    "timelock" -> pure NativeScript
    "plutusV1" -> pure PlutusV1Script
    "plutusV2" -> pure PlutusV2Script
    invalid ->
      Left $ TypeMismatch $
        "language: expected 'native' or 'plutusV{1|2}', got: " <> invalid

--------------------------------------------------------------------------------
-- BlockfrostScriptInfo
--------------------------------------------------------------------------------

newtype BlockfrostScriptInfo = BlockfrostScriptInfo
  { language :: BlockfrostScriptLanguage
  }

scriptLanguage :: BlockfrostScriptInfo -> BlockfrostScriptLanguage
scriptLanguage = _.language <<< unwrap

derive instance Generic BlockfrostScriptInfo _
derive instance Newtype BlockfrostScriptInfo _
derive instance Eq BlockfrostScriptInfo

instance Show BlockfrostScriptInfo where
  show = genericShow

instance DecodeAeson BlockfrostScriptInfo where
  decodeAeson =
    aesonObject (map (wrap <<< { language: _ }) <<< flip getField "type")

--------------------------------------------------------------------------------
-- BlockfrostNativeScript
--------------------------------------------------------------------------------

newtype BlockfrostNativeScript = BlockfrostNativeScript NativeScript

derive instance Generic BlockfrostNativeScript _
derive instance Newtype BlockfrostNativeScript _

instance Show BlockfrostNativeScript where
  show = genericShow

instance DecodeAeson BlockfrostNativeScript where
  decodeAeson =
    aesonObject (flip getField "json") >=> (map wrap <<< decodeNativeScript)
    where
    decodeNativeScript :: Object Aeson -> Either JsonDecodeError NativeScript
    decodeNativeScript obj = getField obj "type" >>= case _ of
      "sig" ->
        ScriptPubkey <$>
          ( getField obj "keyHash" >>=
              (note (TypeMismatch "Ed25519KeyHash") <<< ed25519KeyHashFromBytes)
          )
      "before" ->
        TimelockExpiry <$> getField obj "slot"
      "after" ->
        TimelockStart <$> getField obj "slot"
      "all" ->
        ScriptAll <$> decodeScripts
      "any" ->
        ScriptAny <$> decodeScripts
      "atLeast" ->
        ScriptNOfK <$> getField obj "required" <*> decodeScripts
      _ ->
        Left $ TypeMismatch "Native script constructor"
      where
      decodeScripts :: Either JsonDecodeError (Array NativeScript)
      decodeScripts =
        getField obj "scripts" >>= traverse (aesonObject decodeNativeScript)

--------------------------------------------------------------------------------
-- BlockfrostCbor
--------------------------------------------------------------------------------

newtype BlockfrostCbor = BlockfrostCbor (Maybe ByteArray)

derive instance Generic BlockfrostCbor _
derive instance Newtype BlockfrostCbor _

instance Show BlockfrostCbor where
  show = genericShow

instance DecodeAeson BlockfrostCbor where
  decodeAeson aeson
    | isNull aeson = pure $ BlockfrostCbor Nothing
    | otherwise = do
        cbor <- aesonObject (flip getFieldOptional "cbor") aeson
        pure $ BlockfrostCbor cbor

--------------------------------------------------------------------------------
-- BlockfrostDatum
--------------------------------------------------------------------------------

newtype BlockfrostDatum = BlockfrostDatum (Maybe Datum)

derive instance Generic BlockfrostDatum _
derive instance Newtype BlockfrostDatum _

instance Show BlockfrostDatum where
  show = genericShow

unwrapBlockfrostDatum :: BlockfrostDatum -> Maybe Datum
unwrapBlockfrostDatum = unwrap

instance DecodeAeson BlockfrostDatum where
  decodeAeson aeson
    | isNull aeson = pure $ BlockfrostDatum Nothing
    | otherwise = do
        cbor <- aesonObject (flip getFieldOptional "cbor") aeson
        pure $ BlockfrostDatum $ deserializeData =<< cbor

--------------------------------------------------------------------------------
-- BlockfrostMetadata
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
