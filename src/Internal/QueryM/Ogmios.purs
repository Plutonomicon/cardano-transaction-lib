module Ctl.Internal.QueryM.Ogmios
  ( getSystemStartTime
  , getChainTip
  , currentEpoch
  , submitTxOgmios
  , poolParameters
  , delegationsAndRewards
  , eraSummaries
  , getProtocolParameters
  , evaluateTxOgmios
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson, parseJsonStringToAeson, stringifyAeson)
import Aeson as Aeson
import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax (printError)
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode))
import Cardano.Provider.TxEvaluation as Provider
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.Chain as Chain
import Cardano.Types.TransactionHash (TransactionHash)
import Contract.Log (logTrace')
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.HttpUtils (handleAffjaxResponseGeneric)
import Ctl.Internal.QueryM.Ogmios.Types
  ( class DecodeOgmios
  , AdditionalUtxoSet
  , ChainTipQR(CtChainPoint, CtChainOrigin)
  , CurrentEpoch
  , DelegationsAndRewardsR
  , OgmiosDecodeError(ErrorResponse, InvalidRpcResponse)
  , OgmiosEraSummaries
  , OgmiosError(OgmiosError)
  , OgmiosProtocolParameters
  , OgmiosSystemStart
  , OgmiosTxEvaluationR
  , PoolParametersR
  , StakePoolsQueryArgument
  , SubmitTxR
  , decodeOgmios
  , pprintOgmiosDecodeError
  )
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left), either)
import Data.HTTP.Method (Method(POST))
import Data.Lens (_Right, to, (^?))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)

--------------------------------------------------------------------------------
-- Local State Query Protocol
-- https://ogmios.dev/mini-protocols/local-state-query/
--------------------------------------------------------------------------------
eraSummaries :: QueryM (Either OgmiosDecodeError OgmiosEraSummaries)
eraSummaries = ogmiosQueryNoParams "queryLedgerState/eraSummaries"

getSystemStartTime :: QueryM (Either OgmiosDecodeError OgmiosSystemStart)
getSystemStartTime = ogmiosQueryNoParams "queryNetwork/startTime"

getProtocolParameters
  :: QueryM (Either OgmiosDecodeError OgmiosProtocolParameters)
getProtocolParameters = ogmiosQueryNoParams
  "queryLedgerState/protocolParameters"

getChainTip :: QueryM Chain.Tip
getChainTip = do
  ogmiosChainTipToTip <$> ogmiosErrorHandler
    (ogmiosQueryNoParams "queryNetwork/tip")
  where
  ogmiosChainTipToTip :: ChainTipQR -> Chain.Tip
  ogmiosChainTipToTip = case _ of
    CtChainOrigin _ -> Chain.TipAtGenesis
    CtChainPoint { slot, id } -> Chain.Tip $ wrap
      { slot, blockHeaderHash: wrap $ unwrap id }

currentEpoch :: QueryM (Either OgmiosDecodeError CurrentEpoch)
currentEpoch = ogmiosQueryNoParams "queryLedgerState/epoch"

submitTxOgmios :: TransactionHash -> CborBytes -> QueryM SubmitTxR
submitTxOgmios txHash tx = ogmiosErrorHandlerWithArg submitTx
  (txHash /\ tx)
  where
  submitTx (_ /\ cbor) = ogmiosQueryParams "submitTransaction"
    { transaction:
        { cbor: byteArrayToHex (unwrap cbor)
        }
    }

poolParameters
  :: StakePoolsQueryArgument
  -> QueryM (Either OgmiosDecodeError PoolParametersR)
poolParameters stakePools = ogmiosQueryParams "queryLedgerState/stakePools"
  stakePools

delegationsAndRewards
  :: Array String -- ^ A list of reward account bech32 strings
  -> QueryM (Either OgmiosDecodeError DelegationsAndRewardsR)
delegationsAndRewards rewardAccounts = ogmiosQueryParams
  "queryLedgerState/rewardAccountSummaries"
  { query:
      { delegationsAndRewards: rewardAccounts }
  }

evaluateTxOgmios
  :: CborBytes
  -> AdditionalUtxoSet
  -> QueryM Provider.TxEvaluationR
evaluateTxOgmios cbor additionalUtxos = unwrap <$> ogmiosErrorHandlerWithArg
  evaluateTx
  (cbor /\ additionalUtxos)
  where
  evaluateTx
    :: CborBytes /\ AdditionalUtxoSet
    -> QueryM (Either OgmiosDecodeError OgmiosTxEvaluationR)
  evaluateTx (cbor_ /\ utxoqr) = ogmiosQueryParams "evaluateTransaction"
    { transaction: { cbor: byteArrayToHex $ unwrap cbor_ }
    , additionalUtxo: utxoqr
    }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

ogmiosQueryNoParams
  :: forall a
   . DecodeOgmios a
  => String
  -> QueryM (Either OgmiosDecodeError a)
ogmiosQueryNoParams = flip ogmiosQueryParams {}

ogmiosQueryParams
  :: forall a p
   . DecodeOgmios a
  => EncodeAeson p
  => String
  -> p
  -> QueryM (Either OgmiosDecodeError a)
ogmiosQueryParams method params = do
  let
    body = Aeson.encodeAeson
      { jsonrpc: "2.0"
      , method
      , params
      }
  handleAffjaxOgmiosResponse <$> ogmiosPostRequest body

ogmiosPostRequest
  :: Aeson -- ^ JSON-RPC request body
  -> QueryM (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequest body = do
  config <- asks (_.ogmiosConfig <<< _.config)
  logTrace' $ "sending ogmios HTTP request: " <> show body
  liftAff $ ogmiosPostRequestAff config body

ogmiosPostRequestAff
  :: ServerConfig
  -> Aeson
  -> Aff (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequestAff = ogmiosPostRequestRetryAff (Milliseconds 1000.0)

ogmiosPostRequestRetryAff
  :: Milliseconds
  -> ServerConfig
  -> Aeson
  -> Aff (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequestRetryAff delayMs config body = do
  let
    req = Affjax.defaultRequest
      { method = Left POST
      , url = mkHttpUrl config
      , headers =
          [ Affjax.RequestHeader.RequestHeader "Content-Type"
              "application/json"
          ]
      , content = Just $ Affjax.RequestBody.String $ stringifyAeson body
      , responseFormat = Affjax.ResponseFormat.string
      }

  result <- Affjax.request req

  if result ^? _Right <<< to _.status == Just (StatusCode 503) then
    delay delayMs *>
      ogmiosPostRequestRetryAff (Milliseconds (unwrap delayMs * 2.0)) config
        body

  else pure result

handleAffjaxOgmiosResponse
  :: forall (result :: Type)
   . DecodeOgmios result
  => Either Affjax.Error (Affjax.Response String)
  -> Either OgmiosDecodeError result
handleAffjaxOgmiosResponse =
  handleAffjaxResponseGeneric
    { httpError:
        ( \err -> ErrorResponse $ Just $ OgmiosError
            { code: 0, message: printError err, data: Nothing }
        )
    , httpStatusCodeError:
        ( \code body -> ErrorResponse $ Just $ OgmiosError
            { code, message: "body: " <> body, data: Nothing }
        )
    , decodeError: (\_body jsonErr -> InvalidRpcResponse jsonErr)
    , parse: parseJsonStringToAeson
    , transform: decodeOgmios
    }

ogmiosErrorHandler
  :: forall a m
   . MonadAff m
  => MonadThrow Error m
  => m (Either OgmiosDecodeError a)
  -> m a
ogmiosErrorHandler fun = fun >>= either
  (throwError <<< error <<< pprintOgmiosDecodeError)
  pure

ogmiosErrorHandlerWithArg
  :: forall a m b
   . MonadAff m
  => MonadThrow Error m
  => (a -> m (Either OgmiosDecodeError b))
  -> a
  -> m b
ogmiosErrorHandlerWithArg fun arg = fun arg >>= either
  (throwError <<< error <<< pprintOgmiosDecodeError)
  pure
