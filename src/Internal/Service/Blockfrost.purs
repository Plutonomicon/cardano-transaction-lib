module Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , BlockfrostServiceParams
  , BlockfrostCurrentEpoch(BlockfrostCurrentEpoch)
  , BlockfrostProtocolParameters(BlockfrostProtocolParameters)
  , runBlockfrostServiceM
  , getCurrentEpoch
  , getProtocolParameters
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson, parseJsonStringToAeson)
import Affjax (Error, Response, URL, defaultRequest, request) as Affjax
import Affjax.RequestBody (RequestBody) as Affjax
import Affjax.RequestHeader (RequestHeader(ContentType, RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Ctl.Internal.Contract.QueryBackend (BlockfrostBackend)
import Ctl.Internal.QueryM.Ogmios (PParamRational(..))
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Service.Error (ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError), ServiceError(ServiceBlockfrostError))
import Ctl.Internal.Types.Rational (Rational)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

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
  = GetCurrentEpoch
  | GetProtocolParams

realizeEndpoint :: BlockfrostEndpoint -> Affjax.URL
realizeEndpoint endpoint =
  case endpoint of
    GetCurrentEpoch -> "/epochs/latest"
    GetProtocolParams -> "/epochs/latest/parameters"


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

-- It has the same types as the QueryM.Ogmios.ProtocolParametersRaw
-- The one commented are the types whose blockfrost names I haven't found yet.
type BlockfrostProtocolParametersRaw = {
  "protocol_major_ver": UInt,
  "protocol_minor_ver": UInt,
  "decentralisation_param": Rational,
  "extra_entropy": Maybe Nonce,
  "max_block_header_size": UInt,
  "max_block_size": UInt,
  "max_tx_size": UInt,
-- txFeeFixed
-- txFeePerByte
  "key_deposit": BigInt,
  "pool_deposit": BigInt,
  "min_pool_cost": BigInt,
-- poolRetireMaxEpoch :: Epoch,
-- stakePoolTargetNum :: UInt,
-- poolPledgeInfluence :: Rational,
--  , monetaryExpansion :: Rational
--  , treasuryCut :: Rational
  "coins_per_utxo_size": Maybe BigInt,
  "coins_per_utxo_word": Maybe BigInt
  "cost_models": {
  "PlutusV1": { | Ogmios.CostModelV1 }
,
  "PlutusV2": Maybe { | Ogmios.CostModelV2 }
  },
  "price_mem": PParamRational,
  "price_step": PParamRational,
  "max_tx_ex_mem": BigInt,
  "max_tx_ex_steps": BigInt,
  "max_block_ex_mem": BigInt,
  "max_block_ex_steps": BigInt,
  "max_val_size": UInt,
  "collateral_percent": UInt,
  "max_collateral_inputs": UInt,
  -- From here on I don't know If we need those or what's it name (and type) in the Ogmios.ProtocolParametersRaw
  "epoch": 225,
  "min_fee_a": 44,
  "min_fee_b": 155381,
  "e_max": 18,
  "n_opt": 150,
  "a0": 0.3,
  "rho": 0.003,
  "tau": 0.2,
  "min_utxo": "1000000",
  "nonce": "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81",
}

getCurrentEpoch
  :: BlockfrostServiceM (Either ClientError BlockfrostCurrentEpoch)
getCurrentEpoch = blockfrostGetRequest GetCurrentEpoch
  <#> handleBlockfrostResponse

getProtocolParameters
  :: BlockfrostServiceM (Either ClientError Ogmios.ProtocolParameters)
getProtocolParameters = do
  rawDecoded <- blockfrostGetRequest GetCurrentEpoch 
                  <#> handleBlockfrostResponse
  let 
      maxTxUnits : ExUnits = {memory: rawDecoded.max_tx_ex_mem, steps: rawDecoded.max_tx_ex_steps}
      maxBlocExUnits : ExUnits = {memory: rawDecoded.max_block_ex_mem, steps: rawDecoded.max_block_ex_steps}
      decodePrices ps = note (TypeMismatch "ExUnitPrices") do
        memPrice <- Ogmios.rationalToSubcoin rawDecoded.price_mem
        stepPrice <- Ogmios.rationalToSubcoin rawDecoded.price_step
        pure { memPrice, stepPrice }
