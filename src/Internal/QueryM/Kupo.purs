module Ctl.Internal.QueryM.Kupo
  ( getDatumByHash
  , getScriptByHash
  , getScriptsByHashes
  , getTxMetadata
  , getUtxoByOref
  , isTxConfirmed
  , isTxConfirmedAff
  , utxosAt
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , isNull
  )
import Affjax (Error, Response, defaultRequest, request) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Control.Alt ((<|>))
import Control.Bind (bindFlipped)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Class (asks)
import Control.Parallel (parTraverse)
import Ctl.Internal.Cardano.Types.NativeScript (NativeScript)
import Ctl.Internal.Cardano.Types.ScriptRef
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  )
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoMap
  )
import Ctl.Internal.Cardano.Types.Value
  ( NonAdaAsset
  , Value
  , mkCurrencySymbol
  , mkSingletonNonAdaAsset
  , mkValue
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.NativeScript (convertNativeScript)
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
import Ctl.Internal.Deserialization.Transaction
  ( convertGeneralTransactionMetadata
  )
import Ctl.Internal.QueryM
  ( ClientError(ClientOtherError)
  , QueryM
  , handleAffjaxResponse
  )
import Ctl.Internal.QueryM.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Serialization.Address
  ( Address
  , Slot
  , addressBech32
  , addressFromBech32
  , addressFromBytes
  )
import Ctl.Internal.Serialization.Hash (ScriptHash, scriptHashToBytes)
import Ctl.Internal.Types.BigNum (toString) as BigNum
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes (hexToCborBytes)
import Ctl.Internal.Types.Datum (DataHash(DataHash), Datum)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatumHash, OutputDatum)
  )
import Ctl.Internal.Types.RawBytes (rawBytesToHex)
import Ctl.Internal.Types.Scripts (plutusV1Script, plutusV2Script)
import Ctl.Internal.Types.TokenName (mkTokenName)
import Ctl.Internal.Types.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Ctl.Internal.Types.TransactionMetadata (GeneralTransactionMetadata)
import Data.Array (uncons)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right), hush, note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Map (Map)
import Data.Map (catMaybes, fromFoldable, lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), drop, indexOf, splitAt) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toString) as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

utxosAt :: Address -> QueryM (Either ClientError UtxoMap)
utxosAt address = runExceptT do
  let endpoint = "/matches/" <> addressBech32 address <> "?unspent"
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
  endpoint = "/matches/" <> outputIndex <> "@" <> txHashHex <> "?unspent"
    where
    TransactionInput { transactionId: txHash, index } = oref

    outputIndex :: String
    outputIndex = UInt.toString index

    txHashHex :: String
    txHashHex = byteArrayToHex (unwrap txHash)

getDatumByHash :: DataHash -> QueryM (Either ClientError (Maybe Datum))
getDatumByHash (DataHash dataHashBytes) = do
  let endpoint = "/datums/" <> byteArrayToHex dataHashBytes
  kupoGetRequest endpoint
    <#> map unwrapKupoDatum <<< handleAffjaxResponse

getScriptByHash :: ScriptHash -> QueryM (Either ClientError (Maybe ScriptRef))
getScriptByHash scriptHash = do
  let endpoint = "/scripts/" <> rawBytesToHex (scriptHashToBytes scriptHash)
  kupoGetRequest endpoint
    <#> map unwrapKupoScriptRef <<< handleAffjaxResponse

getScriptsByHashes
  :: Array ScriptHash -> QueryM (Either ClientError (Map ScriptHash ScriptRef))
getScriptsByHashes =
  runExceptT
    <<< map (Map.catMaybes <<< Map.fromFoldable)
    <<< parTraverse (\sh -> Tuple sh <$> ExceptT (getScriptByHash sh))

-- FIXME: This can only confirm transactions with at least one output.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
isTxConfirmed :: TransactionHash -> QueryM (Either ClientError (Maybe Slot))
isTxConfirmed th = do
  config <- asks (_.kupoConfig <<< _.config)
  liftAff $ isTxConfirmedAff config th

-- Exported due to Ogmios requiring confirmations at a websocket level
isTxConfirmedAff
  :: ServerConfig -> TransactionHash -> Aff (Either ClientError (Maybe Slot))
isTxConfirmedAff config (TransactionHash txHash) = do
  let endpoint = "/matches/*@" <> byteArrayToHex txHash
  kupoGetRequestAff config endpoint
    <#> handleAffjaxResponse >>> map \utxos ->
      case uncons (utxos :: _ { created_at :: { slot_no :: Slot } }) of
        Just { head } -> Just head.created_at.slot_no
        _ -> Nothing

getTxMetadata
  :: TransactionHash
  -> QueryM (Either ClientError (Maybe GeneralTransactionMetadata))
getTxMetadata txHash = runExceptT do
  ExceptT (isTxConfirmed txHash) >>= case _ of
    Nothing -> pure Nothing
    Just slot -> do
      let
        endpoint = "/metadata/" <> BigNum.toString (unwrap slot)
          <> "?transaction_id="
          <> byteArrayToHex (unwrap txHash)
      generalTxMetadatas <- ExceptT $ handleAffjaxResponse <$> kupoGetRequest
        endpoint
      pure case uncons (generalTxMetadatas :: _ { raw :: String }) of
        Just { head, tail: [] } ->
          hexToByteArray head.raw >>=
            (fromBytes >=> convertGeneralTransactionMetadata >>> hush)
        _ -> Nothing

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
          (addressFromBech32 x <|> (addressFromBytes =<< hexToCborBytes x))

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
        mkValue coins <<< fold <$> traverse decodeNonAdaAsset assets
      where
      decodeNonAdaAsset
        :: (String /\ BigInt) -> Either JsonDecodeError NonAdaAsset
      decodeNonAdaAsset (assetString /\ assetQuantity) =
        let
          csString /\ tnString =
            case String.indexOf (String.Pattern ".") assetString of
              Nothing ->
                assetString /\ mempty
              Just ix ->
                String.splitAt ix assetString
                  # \{ before, after } -> before /\ String.drop 1 after
        in
          mkSingletonNonAdaAsset
            <$>
              ( note (assetStringTypeMismatch "CurrencySymbol" csString)
                  (mkCurrencySymbol =<< hexToByteArray csString)
              )
            <*>
              ( note (assetStringTypeMismatch "TokenName" tnString)
                  (mkTokenName =<< hexToByteArray tnString)
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
      flip getField "transaction_id" >=> hexToByteArray >>> case _ of
        Nothing -> Left (TypeMismatch "Expected hexstring")
        Just txHashBytes -> pure (TransactionHash txHashBytes)

resolveKupoUtxoMap :: KupoUtxoMap -> QueryM (Either ClientError UtxoMap)
resolveKupoUtxoMap (KupoUtxoMap kupoUtxoMap) =
  runExceptT $ parTraverse (ExceptT <<< resolveKupoTxOutput) kupoUtxoMap

resolveKupoTxOutput
  :: KupoTransactionOutput -> QueryM (Either ClientError TransactionOutput)
resolveKupoTxOutput (KupoTransactionOutput kupoTxOutput@{ address, amount }) =
  runExceptT $
    mkTxOutput <$> ExceptT resolveDatum <*> ExceptT resolveScriptRef
  where
  mkTxOutput :: OutputDatum -> Maybe ScriptRef -> TransactionOutput
  mkTxOutput datum scriptRef =
    TransactionOutput { address, amount, datum, scriptRef }

  resolveDatum :: QueryM (Either ClientError OutputDatum)
  resolveDatum =
    case kupoTxOutput.datumHash of
      Nothing -> pure $ Right NoOutputDatum
      Just (datumHash /\ DatumHash) ->
        pure $ Right $ OutputDatumHash datumHash
      Just (datumHash /\ InlineDatum) -> runExceptT do
        datum <- ExceptT $ getDatumByHash datumHash
        except $ OutputDatum <$> flip note datum
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

newtype KupoDatum = KupoDatum (Maybe Datum)

derive instance Newtype KupoDatum _

unwrapKupoDatum :: KupoDatum -> Maybe Datum
unwrapKupoDatum = unwrap

instance DecodeAeson KupoDatum where
  decodeAeson aeson
    | isNull aeson = pure $ KupoDatum Nothing
    | otherwise =
        aesonObject (flip getFieldOptional "datum") aeson
          >>= pure <<< KupoDatum <<< bindFlipped deserializeData

--------------------------------------------------------------------------------
-- `getScriptByHash` response parsing
--------------------------------------------------------------------------------

data KupoScriptLanguage = NativeScript | PlutusV1Script | PlutusV2Script

derive instance Generic KupoScriptLanguage _

instance Show KupoScriptLanguage where
  show = genericShow

instance DecodeAeson KupoScriptLanguage where
  decodeAeson = aesonString $ case _ of
    "native" -> pure NativeScript
    "plutus:v1" -> pure PlutusV1Script
    "plutus:v2" -> pure PlutusV2Script
    invalid ->
      Left $ TypeMismatch $
        "language: expected 'native' or 'plutus:v{1|2}', got: " <> invalid

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
                NativeScriptRef <$> decodeNativeScript scriptBytes
              PlutusV1Script ->
                pure $ PlutusScriptRef $ plutusV1Script scriptBytes
              PlutusV2Script ->
                pure $ PlutusScriptRef $ plutusV2Script scriptBytes
        where
        decodeNativeScript :: ByteArray -> Either JsonDecodeError NativeScript
        decodeNativeScript scriptBytes = do
          nativeScript <-
            flip note (fromBytes scriptBytes) $
              TypeMismatch "decodeNativeScript: from_bytes() call failed"
          flip note (convertNativeScript nativeScript) $
            TypeMismatch "decodeNativeScript: failed to convert native script"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

kupoGetRequest
  :: String -> QueryM (Either Affjax.Error (Affjax.Response String))
kupoGetRequest endpoint = do
  config <- asks (_.kupoConfig <<< _.config)
  liftAff $ kupoGetRequestAff config endpoint

kupoGetRequestAff
  :: ServerConfig
  -> String
  -> Aff (Either Affjax.Error (Affjax.Response String))
kupoGetRequestAff config endpoint = do
  Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = mkHttpUrl config <> endpoint
    , responseFormat = Affjax.ResponseFormat.string
    }

aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Expected Array"))

aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "Expected Object"))

aesonString
  :: forall (a :: Type)
   . (String -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonString = caseAesonString (Left (TypeMismatch "Expected String"))
