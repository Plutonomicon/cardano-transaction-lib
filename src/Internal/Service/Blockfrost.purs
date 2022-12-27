module Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , BlockfrostServiceParams
  -- TODO: should not be exported:
  , BlockfrostTransactionOutput(BlockfrostTransactionOutput)
  , BlockfrostUnspentOutput
  , BlockfrostUtxosAtAddress(BlockfrostUtxosAtAddress)
  --
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
  , getFieldOptional'
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestHeader (RequestHeader(RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Cardano.Types.Value (Value)
import Ctl.Internal.Cardano.Types.Value
  ( lovelaceValueOf
  , mkSingletonNonAdaAsset
  , mkValue
  ) as Value
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.Deserialization.PlutusData (deserializeData)
import Ctl.Internal.QueryM (ClientError, handleAffjaxResponse)
import Ctl.Internal.QueryM.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Serialization.Address
  ( Address
  , addressBech32
  , addressFromBech32
  )
import Ctl.Internal.Serialization.Hash (ScriptHash)
import Ctl.Internal.Service.Helpers (aesonArray, aesonObject, decodeAssetClass)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.OutputDatum
  ( OutputDatum(NoOutputDatum, OutputDatum, OutputDatumHash)
  )
import Ctl.Internal.Types.Transaction
  ( TransactionHash
  , TransactionInput(TransactionInput)
  )
import Data.Array (find) as Array
import Data.BigInt (fromString) as BigInt
import Data.Either (Either(Left), note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (splitAt) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object)

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

data BlockfrostEndpoint
  = GetUtxosAtAddress Address -- /addresses/{address}/utxos
  | GetUtxosOfTransaction TransactionHash -- /txs/{hash}/utxos

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    GetUtxosAtAddress address ->
      "/addresses/" <> addressBech32 address <> "/utxos"
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

--------------------------------------------------------------------------------
-- Get utxos at address / by output reference
--------------------------------------------------------------------------------

utxosAt
  :: Address
  -- TODO: resolve `BlockfrostUtxosAtAddress`
  -- -> BlockfrostServiceM (Either ClientError UtxoMap)
  -> BlockfrostServiceM (Either ClientError BlockfrostUtxosAtAddress)
utxosAt address =
  handleAffjaxResponse <$>
    blockfrostGetRequest (GetUtxosAtAddress address)

getUtxoByOref
  :: TransactionInput
  -- TODO: resolve `BlockfrostTransactionOutput`
  -- -> BlockfrostServiceM (Either ClientError (Maybe TransactionOutput))
  -> BlockfrostServiceM (Either ClientError (Maybe BlockfrostTransactionOutput))
getUtxoByOref oref@(TransactionInput { transactionId: txHash }) = runExceptT do
  (blockfrostUtxoMap :: BlockfrostUtxosOfTransaction) <-
    ExceptT $ handleAffjaxResponse <$>
      blockfrostGetRequest (GetUtxosOfTransaction txHash)
  pure $ snd <$> Array.find (eq oref <<< fst) (unwrap blockfrostUtxoMap)

--------------------------------------------------------------------------------
-- BlockfrostUtxosAtAddress / BlockfrostUtxosOfTransaction
--------------------------------------------------------------------------------

type BlockfrostUnspentOutput = TransactionInput /\ BlockfrostTransactionOutput

newtype BlockfrostUtxosAtAddress =
  BlockfrostUtxosAtAddress (Array BlockfrostUnspentOutput)

derive instance Generic BlockfrostUtxosAtAddress _
derive instance Newtype BlockfrostUtxosAtAddress _

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

