module Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , BlockfrostServiceParams
  , getChainTip
  , getEraSummaries
  , getSystemStart
  , runBlockfrostServiceM
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , getField
  , getFieldOptional'
  , parseJsonStringToAeson
  )
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Except.Trans (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.QueryM.Ogmios (aesonObject)
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceBlockfrostError)
  )
import Ctl.Internal.Service.Helpers (aesonArray)
import Ctl.Internal.Types.Chain (Tip(Tip, TipAtGenesis))
import Ctl.Internal.Types.EraSummaries
  ( EraSummaries
  , EraSummary
  , EraSummaryParameters
  )
import Ctl.Internal.Types.SystemStart (SystemStart)
import Data.Bifunctor (lmap)
import Data.BigInt (toNumber) as BigInt
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Traversable (traverse)
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
  -- /genesis
  = GetBlockchainGenesis
  -- /network/eras
  | GetEraSummaries
  -- /blocks/latest
  | GetLatestBlock

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    GetBlockchainGenesis -> "/genesis"
    GetEraSummaries -> "/network/eras"
    GetLatestBlock -> "/blocks/latest"

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

--------------------------------------------------------------------------------
-- Get blockchain information
--------------------------------------------------------------------------------

getSystemStart :: BlockfrostServiceM (Either ClientError SystemStart)
getSystemStart = runExceptT do
  (systemStart :: BlockfrostSystemStart) <-
    ExceptT $ handleBlockfrostResponse <$>
      blockfrostGetRequest GetBlockchainGenesis
  pure $ unwrap systemStart

getChainTip :: BlockfrostServiceM (Either ClientError Tip)
getChainTip = runExceptT do
  (chainTip :: BlockfrostChainTip) <-
    ExceptT $ handleBlockfrostResponse <$> blockfrostGetRequest GetLatestBlock
  pure $ unwrap chainTip

getEraSummaries :: BlockfrostServiceM (Either ClientError EraSummaries)
getEraSummaries = runExceptT do
  (eraSummaries :: BlockfrostEraSummaries) <-
    ExceptT $ handleBlockfrostResponse <$> blockfrostGetRequest GetEraSummaries
  pure $ unwrap eraSummaries

--------------------------------------------------------------------------------
-- BlockfrostSystemStart
--------------------------------------------------------------------------------

newtype BlockfrostSystemStart = BlockfrostSystemStart SystemStart

derive instance Generic BlockfrostSystemStart _
derive instance Newtype BlockfrostSystemStart _

instance Show BlockfrostSystemStart where
  show = genericShow

instance DecodeAeson BlockfrostSystemStart where
  decodeAeson = aesonObject \obj -> do
    systemStart <- Seconds <<< BigInt.toNumber <$> getField obj "system_start"
    note (TypeMismatch "Unix timestamp")
      (wrap <<< wrap <<< toDateTime <$> instant (convertDuration systemStart))

--------------------------------------------------------------------------------
-- BlockfrostChainTip
--------------------------------------------------------------------------------

newtype BlockfrostChainTip = BlockfrostChainTip Tip

derive instance Generic BlockfrostChainTip _
derive instance Newtype BlockfrostChainTip _

instance Show BlockfrostChainTip where
  show = genericShow

instance DecodeAeson BlockfrostChainTip where
  decodeAeson = aesonObject \obj -> do
    blockHeaderHash <- wrap <$> getField obj "hash"
    getFieldOptional' obj "slot"
      <#> wrap
        <<< maybe TipAtGenesis (Tip <<< wrap <<< { blockHeaderHash, slot: _ })

--------------------------------------------------------------------------------
-- BlockfrostEraSummaries
--------------------------------------------------------------------------------

newtype BlockfrostEraSummaries = BlockfrostEraSummaries EraSummaries

derive instance Generic BlockfrostEraSummaries _
derive instance Newtype BlockfrostEraSummaries _

instance Show BlockfrostEraSummaries where
  show = genericShow

instance DecodeAeson BlockfrostEraSummaries where
  decodeAeson = aesonArray (map (wrap <<< wrap) <<< traverse decodeEraSummary)
    where
    decodeEraSummary :: Aeson -> Either JsonDecodeError EraSummary
    decodeEraSummary = aesonObject \obj -> do
      start <- getField obj "start"
      end <- getField obj "end"
      parameters <- decodeEraSummaryParameters =<< getField obj "parameters"
      pure $ wrap { start, end, parameters }

    decodeEraSummaryParameters
      :: Object Aeson -> Either JsonDecodeError EraSummaryParameters
    decodeEraSummaryParameters obj = do
      epochLength <- getField obj "epoch_length"
      slotLength <- wrap <$> mul slotLengthFactor <$> getField obj "slot_length"
      safeZone <- getField obj "safe_zone"
      pure $ wrap { epochLength, slotLength, safeZone }
      where
      -- Blockfrost returns `slotLength` in seconds, and we use milliseconds,
      -- so we need to convert between them.
      slotLengthFactor :: Number
      slotLengthFactor = 1000.0
