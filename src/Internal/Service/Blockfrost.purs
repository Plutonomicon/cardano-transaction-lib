module Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint
      ( GetDatumCbor
      , GetNativeScriptByHash
      , GetPlutusScriptCborByHash
      , GetScriptInfo
      )
  , BlockfrostRawResponses
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , blockfrostPostRequest
  , getDatumByHash
  , getScriptByHash
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
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
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
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum)
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts (plutusV1Script, plutusV2Script)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (modify_, new, read) as Ref
import Foreign.Object (Object)
import Partial.Unsafe (unsafePartial)

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
  -- /scripts/datum/{datum_hash}/cbor
  = GetDatumCbor DataHash
  -- /scripts/{script_hash}
  | GetScriptInfo ScriptHash
  -- /scripts/{script_hash}/cbor
  | GetPlutusScriptCborByHash ScriptHash
  -- /scripts/{script_hash}/json
  | GetNativeScriptByHash ScriptHash

derive instance Generic BlockfrostEndpoint _
derive instance Eq BlockfrostEndpoint
derive instance Ord BlockfrostEndpoint

instance Show BlockfrostEndpoint where
  show = genericShow

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    GetDatumCbor (DataHash hashBytes) ->
      "/scripts/datum/" <> byteArrayToHex hashBytes <> "/cbor"
    GetScriptInfo scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash)
    GetPlutusScriptCborByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/cbor"
    GetNativeScriptByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/json"

blockfrostGetRequest
  :: BlockfrostEndpoint
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostGetRequest endpoint = ask >>= \params -> do
  result <-
    liftAff $ Affjax.request $ Affjax.defaultRequest
      { method = Left GET
      , url = mkHttpUrl params.blockfrostConfig <> realizeEndpoint endpoint
      , responseFormat = Affjax.ResponseFormat.string
      , headers =
          maybe mempty (\apiKey -> [ Affjax.RequestHeader "project_id" apiKey ])
            params.blockfrostApiKey
      }
  updateRawResponses result
  pure result
  where
  updateRawResponses
    :: Either Affjax.Error (Affjax.Response String)
    -> BlockfrostServiceM Unit
  updateRawResponses (Right { body: response }) = do
    rawResponses <- asks _.blockfrostRawResponses
    liftEffect $
      traverse_ (Ref.modify_ (Map.insert endpoint response)) rawResponses
  updateRawResponses _ = pure unit

blockfrostPostRequest
  :: BlockfrostEndpoint
  -> MediaType
  -> Maybe Affjax.RequestBody
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostPostRequest endpoint mediaType mbContent =
  ask >>= \params -> liftAff do
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
  response <- blockfrostGetRequest (GetDatumCbor dataHash)
  pure $ handle404AsNothing $ unwrapBlockfrostDatum <$> handleBlockfrostResponse
    response

--------------------------------------------------------------------------------
-- Get script by hash
--------------------------------------------------------------------------------

getScriptByHash
  :: ScriptHash
  -> BlockfrostServiceM (Either ClientError (Maybe ScriptRef))
getScriptByHash scriptHash = runExceptT $ runMaybeT do
  scriptInfo <- MaybeT $ ExceptT getScriptInfo
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
  getScriptInfo
    :: BlockfrostServiceM (Either ClientError (Maybe BlockfrostScriptInfo))
  getScriptInfo = do
    response <- blockfrostGetRequest (GetScriptInfo scriptHash)
    pure $ handle404AsNothing $ handleBlockfrostResponse response

  getNativeScriptByHash
    :: BlockfrostServiceM (Either ClientError (Maybe NativeScript))
  getNativeScriptByHash = runExceptT do
    (nativeScript :: Maybe BlockfrostNativeScript) <- ExceptT do
      response <- blockfrostGetRequest (GetNativeScriptByHash scriptHash)
      pure $ handle404AsNothing $ handleBlockfrostResponse response
    pure $ unwrap <$> nativeScript

  getPlutusScriptCborByHash
    :: BlockfrostServiceM (Either ClientError (Maybe ByteArray))
  getPlutusScriptCborByHash = runExceptT do
    (plutusScriptCbor :: Maybe BlockfrostCbor) <- ExceptT do
      response <- blockfrostGetRequest (GetPlutusScriptCborByHash scriptHash)
      pure $ handle404AsNothing $ handleBlockfrostResponse response
    pure $ join $ unwrap <$> plutusScriptCbor

--------------------------------------------------------------------------------
-- BlockfrostScriptLanguage
--------------------------------------------------------------------------------

data BlockfrostScriptLanguage = NativeScript | PlutusV1Script | PlutusV2Script

derive instance Generic BlockfrostScriptLanguage _

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

instance Show BlockfrostScriptInfo where
  show = genericShow

instance DecodeAeson BlockfrostScriptInfo where
  decodeAeson = aesonObject \obj ->
    getField obj "type"
      <#> \language -> BlockfrostScriptInfo { language }

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

