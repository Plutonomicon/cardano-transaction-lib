module Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getScriptByHash
  , getTxAuxiliaryData
  , getUtxoByOref
  , getOutputAddressesByTxHash
  , isTxConfirmed
  , isTxConfirmedAff
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
  )
import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode))
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Types
  ( Address
  , BigNum
  , DataHash
  , Language(PlutusV3)
  , MultiAsset
  , PlutusData
  , PlutusScript(PlutusScript)
  , ScriptHash
  , Slot
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , UtxoMap
  , Value
  )
import Cardano.Types.Address as Address
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.AuxiliaryData (AuxiliaryData)
import Cardano.Types.BigNum (toString) as BigNum
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef, PlutusScriptRef))
import Cardano.Types.Value as Value
import Contract.Log (logTrace')
import Control.Alt ((<|>))
import Control.Bind (bindFlipped)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Parallel (parTraverse)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Contract.QueryHandle.Error
  ( GetTxMetadataError
      ( GetTxMetadataClientError
      , GetTxMetadataTxNotFoundError
      , GetTxMetadataMetadataEmptyOrMissingError
      )
  )
import Ctl.Internal.QueryM (QueryM, handleAffjaxResponse)
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error (ClientError(ClientOtherError))
import Ctl.Internal.Service.Helpers (aesonArray, aesonObject, aesonString)
import Data.Array (uncons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Lens (_Right, to, (^?))
import Data.Map (Map)
import Data.Map (fromFoldable, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), drop, indexOf, splitAt) as String
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toString) as UInt
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

utxosAt :: Address -> QueryM (Either ClientError UtxoMap)
utxosAt address = runExceptT do
  let endpoint = "/matches/" <> Address.toBech32 address <> "?unspent"
  kupoUtxoMap <- ExceptT $ handleAffjaxResponse <$> kupoGetRequest endpoint
  ExceptT $ resolveKupoUtxoMap kupoUtxoMap

getUtxoByOref
  :: TransactionInput -> QueryM (Either ClientError (Maybe TransactionOutput))
getUtxoByOref oref = runExceptT do
  kupoUtxoMap <- ExceptT $ handleAffjaxResponse <$> kupoGetRequest endpoint
  utxoMap <- ExceptT $ resolveKupoUtxoMap kupoUtxoMap
  pure $ Map.lookup oref utxoMap
  where
  endpoint :: String
  endpoint = "/matches/" <> outputIndex <> "@" <> txHashToHex txHash <>
    "?unspent"
    where
    TransactionInput { transactionId: txHash, index } = oref

    outputIndex :: String
    outputIndex = UInt.toString index

txHashToHex :: TransactionHash -> String
txHashToHex txHash = byteArrayToHex (toBytes $ unwrap txHash)

-- | Specialized function to get addresses only, without resolving script
-- | references. Used internally.
getOutputAddressesByTxHash
  :: TransactionHash -> QueryM (Either ClientError (Array Address))
getOutputAddressesByTxHash txHash = runExceptT do
  (kupoUtxoMap :: KupoUtxoMap) <- ExceptT $ handleAffjaxResponse <$>
    kupoGetRequest endpoint
  pure $ Array.fromFoldable (Map.values $ unwrap kupoUtxoMap) <#>
    unwrap >>> _.address
  where
  endpoint :: String
  endpoint = "/matches/*@" <> txHashToHex txHash <> "?unspent"

getDatumByHash :: DataHash -> QueryM (Either ClientError (Maybe PlutusData))
getDatumByHash dataHash = do
  let endpoint = "/datums/" <> byteArrayToHex (unwrap $ encodeCbor dataHash)
  kupoGetRequest endpoint
    <#> map unwrapKupoDatum <<< handleAffjaxResponse

getScriptByHash :: ScriptHash -> QueryM (Either ClientError (Maybe ScriptRef))
getScriptByHash scriptHash = do
  let
    endpoint = "/scripts/" <> byteArrayToHex (unwrap (encodeCbor scriptHash))
  kupoGetRequest endpoint
    <#> map unwrapKupoScriptRef <<< handleAffjaxResponse

-- FIXME: This can only confirm transactions with at least one output.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
isTxConfirmed :: TransactionHash -> QueryM (Either ClientError (Maybe Slot))
isTxConfirmed txHash = do
  config <- asks (_.kupoConfig <<< _.config)
  do
    -- we don't add `?unspent`, because we only care about existence of UTxOs,
    -- possibly they can be consumed
    let endpoint = "/matches/*@" <> txHashToHex txHash
    -- Do this clumsy special case logging. It's better than sending it silently
    logTrace' $ "sending kupo request: " <> endpoint
  liftAff $ isTxConfirmedAff config txHash

-- Exported due to Ogmios requiring confirmations at a websocket level
isTxConfirmedAff
  :: ServerConfig -> TransactionHash -> Aff (Either ClientError (Maybe Slot))
isTxConfirmedAff config txHash = runExceptT do
  let endpoint = "/matches/*@" <> txHashToHex txHash
  utxos <- ExceptT $ handleAffjaxResponse <$> kupoGetRequestAff config endpoint
  -- Take the first utxo's slot to give the transactions slot
  pure $ uncons utxos <#> _.head >>> unwrapKupoUtxoSlot

getTxAuxiliaryData
  :: TransactionHash
  -> QueryM (Either GetTxMetadataError AuxiliaryData)
getTxAuxiliaryData txHash = runExceptT do
  ExceptT (lmap GetTxMetadataClientError <$> isTxConfirmed txHash) >>= case _ of
    Nothing -> throwError GetTxMetadataTxNotFoundError
    Just slot -> do
      let
        endpoint = "/metadata/" <> BigNum.toString (unwrap slot)
          <> "?transaction_id="
          <> txHashToHex txHash
      kupoAuxData <- ExceptT $
        lmap GetTxMetadataClientError <<< handleAffjaxResponse <$>
          kupoGetRequest
            endpoint
      case unwrapKupoAuxData kupoAuxData of
        Nothing -> throwError GetTxMetadataMetadataEmptyOrMissingError
        Just auxData
          | unwrap auxData == mempty ->
              throwError GetTxMetadataMetadataEmptyOrMissingError
          | otherwise ->
              pure auxData

--------------------------------------------------------------------------------
-- `utxosAt` response parsing
--------------------------------------------------------------------------------

data KupoDatumType = DatumHash | InlineDatum

derive instance Generic KupoDatumType _
derive instance Eq KupoDatumType

instance Show KupoDatumType where
  show = genericShow

instance DecodeAeson KupoDatumType where
  decodeAeson = aesonString $ case _ of
    "hash" -> pure DatumHash
    "inline" -> pure InlineDatum
    invalid ->
      Left $ TypeMismatch $
        "datum_type: expected 'hash' or 'inline', got: " <> invalid

newtype KupoTransactionOutput = KupoTransactionOutput
  { address :: Address
  , amount :: Value
  , datumHash :: Maybe (DataHash /\ KupoDatumType)
  , scriptHash :: Maybe ScriptHash
  }

derive instance Generic KupoTransactionOutput _
derive instance Newtype KupoTransactionOutput _

instance Show KupoTransactionOutput where
  show = genericShow

instance DecodeAeson KupoTransactionOutput where
  decodeAeson = aesonObject \obj -> do
    address <- decodeAddress obj
    amount <- decodeValue obj
    datumHash <- decodeDatumHash obj
    scriptHash <- getFieldOptional' obj "script_hash"
    pure $ wrap { address, amount, datumHash, scriptHash }
    where
    decodeAddress :: Object Aeson -> Either JsonDecodeError Address
    decodeAddress obj =
      getField obj "address" >>= \x ->
        note (TypeMismatch "Expected bech32 or base16 encoded Shelley address")
          ( Address.fromBech32 x <|>
              (decodeCbor <<< wrap =<< hexToByteArray x)
          )

    decodeDatumHash
      :: Object Aeson
      -> Either JsonDecodeError (Maybe (DataHash /\ KupoDatumType))
    decodeDatumHash obj =
      getFieldOptional' obj "datum_hash" >>=
        traverse (\x -> Tuple x <$> getField obj "datum_type")

    decodeValue
      :: Object Aeson
      -> Either JsonDecodeError Value
    decodeValue =
      flip getField "value" >=> aesonObject \obj -> do
        coins <- getField obj "coins"
        assets <-
          getFieldOptional obj "assets"
            <#> fromMaybe mempty <<< map (Object.toUnfoldable :: _ -> Array _)
        multiAsset <-
          note (TypeMismatch "MultiAsset") <<< MultiAsset.sum =<< traverse
            decodeMultiAsset
            assets
        pure $ Value.mkValue coins multiAsset
      where
      decodeMultiAsset
        :: (String /\ BigNum) -> Either JsonDecodeError MultiAsset
      decodeMultiAsset (assetString /\ assetQuantity) =
        let
          csString /\ tnString =
            case String.indexOf (String.Pattern ".") assetString of
              Nothing ->
                assetString /\ mempty
              Just ix ->
                String.splitAt ix assetString
                  # \{ before, after } -> before /\ String.drop 1 after
        in
          MultiAsset.singleton
            <$>
              ( note (assetStringTypeMismatch "ScriptHash" csString)
                  (decodeCbor <<< wrap =<< hexToByteArray csString)
              )
            <*>
              ( note (assetStringTypeMismatch "AssetName" tnString)
                  (mkAssetName =<< hexToByteArray tnString)
              )
            <*> pure assetQuantity
        where
        assetStringTypeMismatch :: String -> String -> JsonDecodeError
        assetStringTypeMismatch t actual =
          TypeMismatch $
            ("In " <> assetString <> ": Expected hex-encoded " <> t)
              <> (", got: " <> actual)

newtype KupoUtxoMap = KupoUtxoMap (Map TransactionInput KupoTransactionOutput)

derive instance Generic KupoUtxoMap _
derive instance Newtype KupoUtxoMap _

instance Show KupoUtxoMap where
  show = genericShow

instance DecodeAeson KupoUtxoMap where
  decodeAeson =
    aesonArray (map (wrap <<< Map.fromFoldable) <<< traverse decodeUtxoEntry)
    where
    decodeUtxoEntry
      :: Aeson
      -> Either JsonDecodeError (TransactionInput /\ KupoTransactionOutput)
    decodeUtxoEntry utxoAeson =
      Tuple <$> decodeTxOref utxoAeson <*> decodeAeson utxoAeson

    decodeTxOref :: Aeson -> Either JsonDecodeError TransactionInput
    decodeTxOref = aesonObject \obj -> do
      transactionId <- decodeTxHash obj
      index <- getField obj "output_index"
      pure $ TransactionInput { transactionId, index }

    decodeTxHash :: Object Aeson -> Either JsonDecodeError TransactionHash
    decodeTxHash =
      flip getField "transaction_id"
        >=> hexToByteArray >>> note (TypeMismatch "Expected hexstring")
        >=> fromBytes
          >>> note (TypeMismatch "Expected TransactionHash")
          >>>
            map TransactionHash

resolveKupoUtxoMap :: KupoUtxoMap -> QueryM (Either ClientError UtxoMap)
resolveKupoUtxoMap (KupoUtxoMap kupoUtxoMap) =
  runExceptT $ parTraverse (ExceptT <<< resolveKupoTxOutput) kupoUtxoMap

resolveKupoTxOutput
  :: KupoTransactionOutput -> QueryM (Either ClientError TransactionOutput)
resolveKupoTxOutput (KupoTransactionOutput kupoTxOutput@{ address, amount }) =
  runExceptT $
    mkTxOutput <$> ExceptT resolveDatum <*> ExceptT resolveScriptRef
  where
  mkTxOutput :: Maybe OutputDatum -> Maybe ScriptRef -> TransactionOutput
  mkTxOutput datum scriptRef =
    TransactionOutput { address, amount, datum, scriptRef }

  resolveDatum :: QueryM (Either ClientError (Maybe OutputDatum))
  resolveDatum =
    case kupoTxOutput.datumHash of
      Nothing -> pure $ Right Nothing
      Just (datumHash /\ DatumHash) ->
        pure $ Right $ Just $ OutputDatumHash datumHash
      Just (datumHash /\ InlineDatum) -> runExceptT do
        datum <- ExceptT $ getDatumByHash datumHash
        except $ pure <<< OutputDatum <$> flip note datum
          (ClientOtherError "Kupo: Failed to resolve inline datum")

  resolveScriptRef :: QueryM (Either ClientError (Maybe ScriptRef))
  resolveScriptRef =
    case kupoTxOutput.scriptHash of
      Nothing -> pure $ Right Nothing
      Just scriptHash -> runExceptT do
        scriptRef <- ExceptT $ getScriptByHash scriptHash
        except $ Just <$> flip note scriptRef
          (ClientOtherError "Kupo: Failed to resolve reference script")

--------------------------------------------------------------------------------
-- `getDatumByHash` response parsing
--------------------------------------------------------------------------------

newtype KupoDatum = KupoDatum (Maybe PlutusData)

derive instance Newtype KupoDatum _

unwrapKupoDatum :: KupoDatum -> Maybe PlutusData
unwrapKupoDatum = unwrap

instance DecodeAeson KupoDatum where
  decodeAeson aeson
    | isNull aeson = pure $ KupoDatum Nothing
    | otherwise =
        aesonObject (flip getFieldOptional "datum") aeson
          >>= pure <<< KupoDatum <<< bindFlipped decodeCbor

--------------------------------------------------------------------------------
-- `getScriptByHash` response parsing
--------------------------------------------------------------------------------

data KupoScriptLanguage
  = NativeScript
  | PlutusV1Script
  | PlutusV2Script
  | PlutusV3Script

derive instance Generic KupoScriptLanguage _

instance Show KupoScriptLanguage where
  show = genericShow

instance DecodeAeson KupoScriptLanguage where
  decodeAeson = aesonString $ case _ of
    "native" -> pure NativeScript
    "plutus:v1" -> pure PlutusV1Script
    "plutus:v2" -> pure PlutusV2Script
    "plutus:v3" -> pure PlutusV3Script
    invalid ->
      Left $ TypeMismatch $
        "language: expected 'native' or 'plutus:v{1|2|3}', got: " <> invalid

newtype KupoScriptRef = KupoScriptRef (Maybe ScriptRef)

derive instance Newtype KupoScriptRef _

unwrapKupoScriptRef :: KupoScriptRef -> Maybe ScriptRef
unwrapKupoScriptRef = unwrap

instance DecodeAeson KupoScriptRef where
  decodeAeson aeson
    | isNull aeson = pure $ KupoScriptRef Nothing
    | otherwise =
        aeson # aesonObject \obj -> do
          language <- getField obj "language"
          scriptBytes <- getField obj "script"
          KupoScriptRef <<< Just <$>
            case language of
              NativeScript ->
                NativeScriptRef <$>
                  note (TypeMismatch "NativeScript") (decodeCbor scriptBytes)
              PlutusV1Script ->
                pure $ PlutusScriptRef $ PlutusScript.plutusV1Script $ wrap $
                  unwrap scriptBytes
              PlutusV2Script ->
                pure $ PlutusScriptRef $ PlutusScript.plutusV2Script $ wrap $
                  unwrap scriptBytes
              PlutusV3Script ->
                -- TODO: add plutusV3Script to Cardano.Types.PlutusScript
                pure $ PlutusScriptRef $ PlutusScript $ unwrap scriptBytes /\
                  PlutusV3

-------------------------------------------------------------------------------
-- `isTxConfirmed` response parsing
-------------------------------------------------------------------------------

newtype KupoUtxoSlot = KupoUtxoSlot Slot

derive instance Generic KupoUtxoSlot _
derive instance Eq KupoUtxoSlot

instance Show KupoUtxoSlot where
  show = genericShow

instance DecodeAeson KupoUtxoSlot where
  decodeAeson = decodeAeson >>> map (slot >>> KupoUtxoSlot)
    where
    slot :: { created_at :: { slot_no :: Slot } } -> Slot
    slot = _.created_at.slot_no

unwrapKupoUtxoSlot :: KupoUtxoSlot -> Slot
unwrapKupoUtxoSlot (KupoUtxoSlot slot) = slot

--------------------------------------------------------------------------------
-- `getTxAuxiliaryData` response parsing
--------------------------------------------------------------------------------

newtype KupoAuxiliaryData = KupoAuxiliaryData (Maybe AuxiliaryData)

derive instance Generic KupoAuxiliaryData _
derive instance Eq KupoAuxiliaryData

instance Show KupoAuxiliaryData where
  show = genericShow

instance DecodeAeson KupoAuxiliaryData where
  decodeAeson = decodeAeson >=> case _ of
    [ { raw: cbor } :: { raw :: CborBytes } ] -> do
      auxData <- note (TypeMismatch "Hexadecimal encoded AuxiliaryData") $
        decodeCbor cbor
      pure $ KupoAuxiliaryData $ Just auxData
    [] -> Right $ KupoAuxiliaryData Nothing
    _ -> Left $ TypeMismatch "Singleton or Empty Array"

unwrapKupoAuxData :: KupoAuxiliaryData -> Maybe AuxiliaryData
unwrapKupoAuxData (KupoAuxiliaryData mAuxData) = mAuxData

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

kupoGetRequest
  :: String -> QueryM (Either Affjax.Error (Affjax.Response String))
kupoGetRequest endpoint = do
  config <- asks (_.kupoConfig <<< _.config)
  logTrace' $ "sending kupo request: " <> endpoint
  liftAff $ kupoGetRequestAff config endpoint

kupoGetRequestAff
  :: ServerConfig
  -> String
  -> Aff (Either Affjax.Error (Affjax.Response String))
kupoGetRequestAff = kupoGetRequestRetryAff (Milliseconds 1000.0)

-- | Retry on `503 Service Unavailable` error with exponentially-increasing
-- | interval (to not DOS the service even more).
kupoGetRequestRetryAff
  :: Milliseconds
  -> ServerConfig
  -> String
  -> Aff (Either Affjax.Error (Affjax.Response String))
kupoGetRequestRetryAff delayMs config endpoint = do
  result <- Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = mkHttpUrl config <> endpoint
    , responseFormat = Affjax.ResponseFormat.string
    }
  if result ^? _Right <<< to _.status == Just (StatusCode 503) then
    delay delayMs *>
      kupoGetRequestRetryAff (Milliseconds (unwrap delayMs * 2.0)) config
        endpoint
  else pure result
