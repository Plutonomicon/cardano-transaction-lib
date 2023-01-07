module Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , BlockfrostServiceParams
  -- TODO: should not be exported:
  , BlockfrostEndpoint
  , BlockfrostTransactionOutput(BlockfrostTransactionOutput)
  , BlockfrostUnspentOutput
  , BlockfrostUtxosAtAddress(BlockfrostUtxosAtAddress)
  , blockfrostPostRequest
  --
  , getDatumByHash
  , getScriptByHash
  , getUtxoByOref
  , runBlockfrostServiceM
  , utxosAt
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
import Affjax.RequestHeader
  ( RequestHeader(ContentType, RequestHeader)
  ) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader.Class (ask)
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
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Cardano.Types.Value
  ( lovelaceValueOf
  , mkSingletonNonAdaAsset
  , mkValue
  ) as Value
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
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
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Service.Helpers
  ( aesonArray
  , aesonObject
  , aesonString
  , decodeAssetClass
  )
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex)
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
import Data.Array (find, length) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (fromString) as BigInt
import Data.Either (Either(Left, Right), note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (splitAt) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)

--------------------------------------------------------------------------------
-- BlockfrostServiceM
--------------------------------------------------------------------------------

type BlockfrostServiceParams =
  { blockfrostConfig :: ServerConfig
  , blockfrostApiKey :: Maybe String
  }

type BlockfrostServiceM (a :: Type) = ReaderT BlockfrostServiceParams Aff a

runBlockfrostServiceM
  :: forall (a :: Type). BlockfrostBackend -> BlockfrostServiceM a -> Aff a
runBlockfrostServiceM backend = flip runReaderT serviceParams
  where
  serviceParams :: BlockfrostServiceParams
  serviceParams =
    { blockfrostConfig: backend.blockfrostConfig
    , blockfrostApiKey: backend.blockfrostApiKey
    }

--------------------------------------------------------------------------------
-- Making requests to Blockfrost endpoints
--------------------------------------------------------------------------------

data BlockfrostEndpoint
  -- /scripts/datum/{datum_hash}/cbor
  = GetDatumCbor DataHash
  -- /scripts/{script_hash}/json
  | GetNativeScriptByHash ScriptHash
  -- /scripts/{script_hash}/cbor
  | GetPlutusScriptCborByHash ScriptHash
  -- /scripts/{script_hash}
  | GetScriptInfo ScriptHash
  -- /addresses/{address}/utxos?page={page}&count={count}
  | GetUtxosAtAddress Address Int Int
  -- /txs/{hash}/utxos
  | GetUtxosOfTransaction TransactionHash

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    GetDatumCbor (DataHash hashBytes) ->
      "/scripts/datum/" <> byteArrayToHex hashBytes <> "/cbor"
    GetNativeScriptByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/json"
    GetPlutusScriptCborByHash scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash) <> "/cbor"
    GetScriptInfo scriptHash ->
      "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash)
    GetUtxosAtAddress address page count ->
      "/addresses/" <> addressBech32 address <> "/utxos?page=" <> show page
        <> ("&count=" <> show count)
    GetUtxosOfTransaction txHash ->
      "/txs/" <> byteArrayToHex (unwrap txHash) <> "/utxos"

blockfrostGetRequest
  :: BlockfrostEndpoint
  -> BlockfrostServiceM (Either Affjax.Error (Affjax.Response String))
blockfrostGetRequest endpoint = ask >>= \params -> liftAff do
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
-- Get utxos at address / by output reference
--------------------------------------------------------------------------------

utxosAt
  :: Address
  -- TODO: resolve `BlockfrostUtxosAtAddress`
  -- -> BlockfrostServiceM (Either ClientError UtxoMap)
  -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
utxosAt address = utxosAtAddressOnPage 1
  where
  utxosAtAddressOnPage
    :: Int -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
  utxosAtAddressOnPage page = runExceptT do
    -- Maximum number of results per page supported by Blockfrost:
    let maxNumResultsOnPage = 100
    utxos <- ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest (GetUtxosAtAddress address page maxNumResultsOnPage)
    case Array.length (unwrap utxos) < maxNumResultsOnPage of
      true -> pure utxos
      false -> append utxos <$> ExceptT (utxosAtAddressOnPage $ page + 1)

getUtxoByOref
  :: TransactionInput
  -- TODO: resolve `BlockfrostTransactionOutput`
  -- -> BlockfrostServiceM (Either ClientError (Maybe TransactionOutput))
  -> BlockfrostServiceM (Either ClientError (Maybe BlockfrostTransactionOutput))
getUtxoByOref oref@(TransactionInput { transactionId: txHash }) = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <-
    ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest (GetUtxosOfTransaction txHash)
  pure $ snd <$> Array.find (eq oref <<< fst) (unwrap blockfrostUtxoMap)

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

--------------------------------------------------------------------------------
-- BlockfrostScriptLanguage
--------------------------------------------------------------------------------

data BlockfrostScriptLanguage = NativeScript | PlutusV1Script | PlutusV2Script

derive instance Generic BlockfrostScriptLanguage _

instance Show BlockfrostScriptLanguage where
  show = genericShow

instance DecodeAeson BlockfrostScriptLanguage where
  decodeAeson = aesonString $ case _ of
    "native" -> pure NativeScript
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
    unwrap' :: BlockfrostNativeScript -> NativeScript
    unwrap' = unwrap

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
        ScriptAll <$> map unwrap' <$> getField obj "scripts"
      "any" ->
        ScriptAny <$> map unwrap' <$> getField obj "scripts"
      "atLeast" -> do
        required <- getField obj "required"
        ScriptNOfK required <$> map unwrap' <$> getField obj "scripts"
      _ ->
        Left $ TypeMismatch "Native script constructor"

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

