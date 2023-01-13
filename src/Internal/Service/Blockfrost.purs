module Ctl.Internal.Service.Blockfrost
  ( BlockfrostEndpoint
      ( DatumCbor
      , NativeScriptByHash
      , PlutusScriptCborByHash
      , ScriptInfo
      , Transaction
      , TransactionMetadata
      , UtxosAtAddress
      , UtxosOfTransaction
      )
  , BlockfrostMetadata(BlockfrostMetadata)
  , BlockfrostNativeScript(BlockfrostNativeScript)
  , BlockfrostRawPostResponseData
  , BlockfrostRawResponse
  , BlockfrostScriptInfo(BlockfrostScriptInfo)
  , BlockfrostScriptLanguage(NativeScript, PlutusV1Script, PlutusV2Script)
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , OnBlockfrostRawGetResponseHook
  , OnBlockfrostRawPostResponseHook
  , getDatumByHash
  , getScriptByHash
  , getScriptInfo
  , getTxMetadata
  , getUtxoByOref
  , isTxConfirmed
  , runBlockfrostServiceM
  , runBlockfrostServiceTestM
  , utxosAt
  , dummyExport
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  , parseJsonStringToAeson
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Parallel (parTraverse)
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
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  )
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Cardano.Types.Value
  ( lovelaceValueOf
  , mkSingletonNonAdaAsset
  , mkValue
  ) as Value
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
import Ctl.Internal.Serialization.Address
  ( Address
  , addressBech32
  , addressFromBech32
  )
import Ctl.Internal.Serialization.Hash
  ( ScriptHash
  , ed25519KeyHashFromBytes
  , scriptHashToBytes
  )
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError
      ( ClientDecodeJsonError
      , ClientHttpError
      , ClientHttpResponseError
      , ClientOtherError
      )
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Service.Helpers
  ( aesonArray
  , aesonObject
  , aesonString
  , decodeAssetClass
  )
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatum, OutputDatumHash)
  )
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts (plutusV1Script, plutusV2Script)
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Array (find, length) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (fromString) as BigInt
import Data.Either (Either(Left, Right), note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Map (fromFoldable, isEmpty, unions) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (splitAt) as String
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
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
  -- /addresses/{address}/utxos?page={page}&count={count}
  | UtxosAtAddress Address Int Int
  -- /txs/{hash}/utxos
  | UtxosOfTransaction TransactionHash

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
    UtxosAtAddress address page count ->
      "/addresses/" <> addressBech32 address <> "/utxos?page=" <> show page
        <> ("&count=" <> show count)
    UtxosOfTransaction txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash) <> "/utxos"

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
-- Get utxos at address / by output reference
--------------------------------------------------------------------------------

utxosAt :: Address -> BlockfrostServiceM (Either ClientError UtxoMap)
utxosAt address = runExceptT $
  ExceptT (utxosAtAddressOnPage 1)
    >>= (ExceptT <<< resolveBlockfrostUtxosAtAddress)
  where
  utxosAtAddressOnPage
    :: Int -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
  utxosAtAddressOnPage page = runExceptT do
    -- Maximum number of results per page supported by Blockfrost:
    let maxNumResultsOnPage = 100
    utxos <- ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest (UtxosAtAddress address page maxNumResultsOnPage)
    case Array.length (unwrap utxos) < maxNumResultsOnPage of
      true -> pure utxos
      false -> append utxos <$> ExceptT (utxosAtAddressOnPage $ page + 1)

getUtxoByOref
  :: TransactionInput
  -> BlockfrostServiceM (Either ClientError (Maybe TransactionOutput))
getUtxoByOref oref@(TransactionInput { transactionId: txHash }) = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <-
    ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest (UtxosOfTransaction txHash)
  traverse (ExceptT <<< resolveBlockfrostTxOutput)
    (snd <$> Array.find (eq oref <<< fst) (unwrap blockfrostUtxoMap))

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
-- BlockfrostUtxosAtAddress / BlockfrostUtxosOfTransaction
--------------------------------------------------------------------------------

type BlockfrostUnspentOutput = TransactionInput /\ BlockfrostTransactionOutput

newtype BlockfrostUtxosAtAddress =
  BlockfrostUtxosAtAddress (Array BlockfrostUnspentOutput)

derive instance Generic BlockfrostUtxosAtAddress _
derive instance Newtype BlockfrostUtxosAtAddress _
derive newtype instance Semigroup BlockfrostUtxosAtAddress

instance Show BlockfrostUtxosAtAddress where
  show = genericShow

instance DecodeAeson BlockfrostUtxosAtAddress where
  decodeAeson = aesonArray (map wrap <<< traverse decodeUtxoEntry)
    where
    decodeUtxoEntry :: Aeson -> Either JsonDecodeError BlockfrostUnspentOutput
    decodeUtxoEntry utxoAeson =
      Tuple <$> decodeTxOref utxoAeson <*> decodeAeson utxoAeson

    decodeTxOref :: Aeson -> Either JsonDecodeError TransactionInput
    decodeTxOref = aesonObject \obj -> do
      transactionId <- getField obj "tx_hash"
      index <- getField obj "output_index"
      pure $ TransactionInput { transactionId, index }

resolveBlockfrostUtxosAtAddress
  :: BlockfrostUtxosAtAddress
  -> BlockfrostServiceM (Either ClientError UtxoMap)
resolveBlockfrostUtxosAtAddress (BlockfrostUtxosAtAddress utxos) =
  runExceptT $ Map.fromFoldable <$>
    parTraverse (traverse (ExceptT <<< resolveBlockfrostTxOutput)) utxos

newtype BlockfrostUtxosOfTransaction =
  BlockfrostUtxosOfTransaction (Array BlockfrostUnspentOutput)

derive instance Generic BlockfrostUtxosOfTransaction _
derive instance Newtype BlockfrostUtxosOfTransaction _

instance Show BlockfrostUtxosOfTransaction where
  show = genericShow

instance DecodeAeson BlockfrostUtxosOfTransaction where
  decodeAeson = aesonObject \obj -> do
    txHash <- getField obj "hash"
    getField obj "outputs"
      >>= aesonArray (map wrap <<< traverse (decodeUtxoEntry txHash))
    where
    decodeUtxoEntry
      :: TransactionHash
      -> Aeson
      -> Either JsonDecodeError BlockfrostUnspentOutput
    decodeUtxoEntry txHash utxoAeson =
      Tuple <$> decodeTxOref txHash utxoAeson <*> decodeAeson utxoAeson

    decodeTxOref
      :: TransactionHash -> Aeson -> Either JsonDecodeError TransactionInput
    decodeTxOref txHash = aesonObject $
      flip getField "output_index" >>> map \index ->
        TransactionInput { transactionId: txHash, index }

--------------------------------------------------------------------------------
-- BlockfrostTransactionOutput
--------------------------------------------------------------------------------

newtype BlockfrostTransactionOutput = BlockfrostTransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , scriptHash :: Maybe ScriptHash
  }

derive instance Generic BlockfrostTransactionOutput _
derive instance Newtype BlockfrostTransactionOutput _

instance Show BlockfrostTransactionOutput where
  show = genericShow

instance DecodeAeson BlockfrostTransactionOutput where
  decodeAeson = aesonObject \obj -> do
    address <- decodeAddress obj
    amount <- decodeValue obj
    datum <- decodeOutputDatum obj
    scriptHash <- getFieldOptional' obj "reference_script_hash"
    pure $ wrap { address, amount, datum, scriptHash }
    where
    decodeAddress :: Object Aeson -> Either JsonDecodeError Address
    decodeAddress obj =
      getField obj "address" >>= \address ->
        note (TypeMismatch "Expected bech32 encoded address")
          (addressFromBech32 address)

    decodeValue :: Object Aeson -> Either JsonDecodeError Value
    decodeValue =
      flip getField "amount" >=> aesonArray (map fold <<< traverse decodeAsset)
      where
      decodeAsset :: Aeson -> Either JsonDecodeError Value
      decodeAsset = aesonObject \obj -> do
        quantity <-
          getField obj "quantity" >>=
            BigInt.fromString >>>
              note (TypeMismatch "Expected string repr of BigInt")
        getField obj "unit" >>= case _ of
          "lovelace" -> pure $ Value.lovelaceValueOf quantity
          assetString -> do
            let { before: csStr, after: tnStr } = String.splitAt 56 assetString
            decodeAssetClass assetString csStr tnStr <#> \(cs /\ tn) ->
              Value.mkValue mempty $ Value.mkSingletonNonAdaAsset cs tn quantity

    decodeOutputDatum :: Object Aeson -> Either JsonDecodeError OutputDatum
    decodeOutputDatum obj =
      getFieldOptional' obj "inline_datum" >>= case _ of
        Just datum ->
          note (TypeMismatch "Expected CBOR encoded inline datum")
            (OutputDatum <$> deserializeData datum)
        Nothing ->
          maybe NoOutputDatum OutputDatumHash
            <$> getFieldOptional' obj "data_hash"

resolveBlockfrostTxOutput
  :: BlockfrostTransactionOutput
  -> BlockfrostServiceM (Either ClientError TransactionOutput)
resolveBlockfrostTxOutput
  (BlockfrostTransactionOutput blockfrostTxOutput@{ address, amount, datum }) =
  map mkTxOutput <$> resolveScriptRef
  where
  mkTxOutput :: Maybe ScriptRef -> TransactionOutput
  mkTxOutput scriptRef =
    TransactionOutput { address, amount, datum, scriptRef }

  resolveScriptRef :: BlockfrostServiceM (Either ClientError (Maybe ScriptRef))
  resolveScriptRef =
    case blockfrostTxOutput.scriptHash of
      Nothing -> pure $ Right Nothing
      Just scriptHash -> runExceptT do
        scriptRef <- ExceptT $ getScriptByHash scriptHash
        except $ Just <$> flip note scriptRef
          (ClientOtherError "Blockfrost: Failed to resolve reference script")

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

newtype BlockfrostMetadata = BlockfrostMetadata GeneralTransactionMetadata

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
