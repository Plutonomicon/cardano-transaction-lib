module Ctl.Internal.Service.Blockfrost
  ( isTxConfirmed
  , getTxMetadata
  , BlockfrostMetadata(BlockfrostMetadata)
  , BlockfrostServiceM
  , BlockfrostServiceParams
  , BlockfrostCurrentEpoch(BlockfrostCurrentEpoch)
  , BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  , runBlockfrostServiceM
  , getCurrentEpoch
  , getProtocolParameters
  , dummyExport
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , Finite
  , JsonDecodeError(TypeMismatch, AtKey, MissingValue)
  , decodeAeson
  , decodeJsonString
  , parseJsonStringToAeson
  , unpackFinite
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Cardano.Types.Transaction (Costmdls(..))
import Ctl.Internal.Cardano.Types.Value (Coin(..))
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
import Ctl.Internal.QueryM.Ogmios
  ( CoinsPerUtxoUnit(..)
  , CostModelV1
  , CostModelV2
  , Epoch(..)
  , ProtocolParameters(..)
  , convertCostModel
  , rationalToSubcoin
  )
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceBlockfrostError)
  )
-- import Ctl.Internal.QueryM (handleAffjaxResponse)
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Rational (Rational, reduce)
import Ctl.Internal.Types.Scripts (Language(..))
import Ctl.Internal.Types.Transaction (TransactionHash)
import Ctl.Internal.Types.TransactionMetadata
  ( GeneralTransactionMetadata(GeneralTransactionMetadata)
  )
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.BigNumber (BigNumber, toFraction)
import Data.BigNumber as BigNumber
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Undefined (undefined)

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
  = LatestEpoch
  | LatestProtocolParameters
  | Transaction TransactionHash
  | TransactionMetadata TransactionHash

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    LatestEpoch -> "/epochs/latest"
    LatestProtocolParameters -> "/epochs/latest/parameters"
    Transaction txHash -> "/txs/" <> byteArrayToHex (unwrap txHash)
    TransactionMetadata txHash -> "/txs/" <> byteArrayToHex (unwrap txHash)
      <> "/metadata/cbor"

dummyExport :: Unit -> Unit
dummyExport _ = undefined blockfrostPostRequest

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

newtype BlockfrostCurrentEpoch = BlockfrostCurrentEpoch { epoch :: BigInt }

derive instance Generic BlockfrostCurrentEpoch _
derive instance Newtype BlockfrostCurrentEpoch _
derive newtype instance DecodeAeson BlockfrostCurrentEpoch

instance Show BlockfrostCurrentEpoch where
  show = genericShow

-- | `Stringed a` decodes an `a` who was encoded as a `String`
newtype Stringed a = Stringed a

derive instance Newtype (Stringed a) _

instance DecodeAeson a => DecodeAeson (Stringed a) where
  decodeAeson = decodeAeson >=> decodeJsonString >=> Stringed >>> pure

type BlockfrostProtocolParametersRaw =
  { "min_fee_a" :: UInt
  , "min_fee_b" :: UInt
  , "max_block_size" :: UInt
  , "max_tx_size" :: UInt
  , "max_block_header_size" :: UInt
  , "key_deposit" :: Stringed BigInt
  , "pool_deposit" :: Stringed BigInt
  , "e_max" :: BigInt
  , "n_opt" :: UInt
  , "a0" :: Finite BigNumber
  , "rho" :: Finite BigNumber
  , "tau" :: Finite BigNumber
  , "protocol_major_ver" :: UInt
  , "protocol_minor_ver" :: UInt
  , "min_pool_cost" :: Stringed BigInt
  , "cost_models" ::
      { "PlutusV1" :: { | CostModelV1 }
      , "PlutusV2" :: { | CostModelV2 }
      }
  , "price_mem" :: Finite BigNumber
  , "price_step" :: Finite BigNumber
  , "max_tx_ex_mem" :: Stringed BigInt
  , "max_tx_ex_steps" :: Stringed BigInt
  , "max_block_ex_mem" :: Stringed BigInt
  , "max_block_ex_steps" :: Stringed BigInt
  , "max_val_size" :: Stringed UInt
  , "collateral_percent" :: UInt
  , "max_collateral_inputs" :: UInt
  , "coins_per_utxo_size" :: Maybe (Stringed BigInt)
  , "coins_per_utxo_word" :: Maybe (Stringed BigInt)
  }

bigNumberToRational :: BigNumber -> Maybe Rational
bigNumberToRational bn = do
  let
    (numerator' /\ denominator') = toFraction bn (BigNumber.fromNumber infinity)
  numerator <- BigInt.fromString $ BigNumber.toString numerator'
  denominator <- BigInt.fromString $ BigNumber.toString denominator'
  reduce numerator denominator

bigNumberToRational' :: BigNumber -> Either JsonDecodeError Rational
bigNumberToRational' = note (TypeMismatch "Rational") <<< bigNumberToRational

newtype BlockfrostProtocolParameters =
  BlockfrostProtocolParameters ProtocolParameters

instance DecodeAeson BlockfrostProtocolParameters where
  decodeAeson = decodeAeson >=> \(raw :: BlockfrostProtocolParametersRaw) -> do
    poolPledgeInfluence <- bigNumberToRational' $ unpackFinite raw.a0
    monetaryExpansion <- bigNumberToRational' $ unpackFinite raw.rho
    treasuryCut <- bigNumberToRational' $ unpackFinite raw.tau
    prices <- do
      let
        convert bn = do
          rational <- bigNumberToRational $ unpackFinite bn
          rationalToSubcoin $ wrap rational

      memPrice <- note (TypeMismatch "Rational") $ convert raw.price_mem
      stepPrice <- note (TypeMismatch "Rational") $ convert raw.price_step
      pure { memPrice, stepPrice }

    coinsPerUtxoUnit <-
      maybe
        (Left $ AtKey "coinsPerUtxoByte or coinsPerUtxoWord" $ MissingValue)
        pure
        $ (CoinsPerUtxoByte <<< Coin <<< unwrap <$> raw.coins_per_utxo_size) <|>
            (CoinsPerUtxoWord <<< Coin <<< unwrap <$> raw.coins_per_utxo_word)

    pure $ BlockfrostProtocolParameters $ ProtocolParameters
      { protocolVersion: raw.protocol_major_ver /\ raw.protocol_minor_ver
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , extraPraosEntropy: Nothing
      , maxBlockHeaderSize: raw.max_block_header_size
      , maxBlockBodySize: raw.max_block_size
      , maxTxSize: raw.max_tx_size
      , txFeeFixed: raw.min_fee_b
      , txFeePerByte: raw.min_fee_a
      , stakeAddressDeposit: Coin $ unwrap raw.key_deposit
      , stakePoolDeposit: Coin $ unwrap raw.pool_deposit
      , minPoolCost: Coin $ unwrap raw.min_pool_cost
      , poolRetireMaxEpoch: Epoch raw.e_max
      , stakePoolTargetNum: raw.n_opt
      , poolPledgeInfluence
      , monetaryExpansion
      , treasuryCut
      , coinsPerUtxoUnit: coinsPerUtxoUnit
      , costModels: Costmdls $ Map.fromFoldable
          [ PlutusV1 /\ convertCostModel raw.cost_models."PlutusV1"
          , PlutusV2 /\ convertCostModel raw.cost_models."PlutusV2"
          ]
      , prices
      , maxTxExUnits:
          { mem: unwrap raw.max_tx_ex_mem
          , steps: unwrap raw.max_tx_ex_steps
          }
      , maxBlockExUnits:
          { mem: unwrap raw.max_block_ex_mem
          , steps: unwrap raw.max_block_ex_steps
          }
      , maxValueSize: unwrap raw.max_val_size
      , collateralPercent: raw.collateral_percent
      , maxCollateralInputs: raw.max_collateral_inputs
      }

getCurrentEpoch
  :: BlockfrostServiceM (Either ClientError BigInt)
getCurrentEpoch = runExceptT do
  BlockfrostCurrentEpoch { epoch } <- ExceptT $ blockfrostGetRequest LatestEpoch
    <#> handleBlockfrostResponse
  pure epoch

getProtocolParameters
  :: BlockfrostServiceM (Either ClientError Ogmios.ProtocolParameters)
getProtocolParameters = runExceptT do
  BlockfrostProtocolParameters params <- ExceptT $
    blockfrostGetRequest LatestProtocolParameters <#> handleBlockfrostResponse
  pure params

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
