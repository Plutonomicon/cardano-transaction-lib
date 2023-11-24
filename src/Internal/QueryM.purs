-- | CTL query layer monad.
-- | This module defines an Aff interface for Ogmios Websocket Queries.
-- | Since WebSockets do not define a mechanism for linking request/response.
-- | Or for verifying that the connection is live, those concerns are addressed here
module Ctl.Internal.QueryM
  ( module ExportDispatcher
  , module ExportServerConfig
  , ClusterSetup
  , ListenerSet
  , OgmiosListeners
  , OgmiosWebSocket
  , QueryConfig
  , QueryM
  , ParQueryM
  , QueryMT(QueryMT)
  , QueryEnv
  , QueryRuntime
  , SubmitTxListenerSet
  , WebSocket(WebSocket)
  , acquireMempoolSnapshot
  , acquireMempoolSnapshotAff
  , evaluateTxOgmios
  , getChainTip
  , getLogger
  , getProtocolParametersAff
  , getSystemStartAff
  , handleAffjaxResponse
  , listeners
  , postAeson
  , mkListenerSet
  , defaultMessageListener
  , mempoolSnapshotHasTx
  , mempoolSnapshotHasTxAff
  , mempoolSnapshotNextTx
  , mempoolSnapshotNextTxAff
  , mempoolSnapshotSizeAndCapacity
  , mempoolSnapshotSizeAndCapacityAff
  , mkOgmiosRequest
  , mkOgmiosRequestAff
  , mkOgmiosWebSocketAff
  , mkRequest
  , mkRequestAff
  , releaseMempool
  , releaseMempoolAff
  , scriptToAeson
  , submitTxOgmios
  , underlyingWebSocket
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  , stringifyAeson
  )
import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.StatusCode as Affjax.StatusCode
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , liftEither
  , throwError
  )
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (class Parallel, parallel, sequential)
import Control.Plus (class Plus)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.JsWebSocket
  ( JsWebSocket
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _removeOnWsError
  , _wsClose
  , _wsFinalize
  , _wsSend
  )
import Ctl.Internal.Logging (Logger, mkLogger)
import Ctl.Internal.QueryM.Dispatcher
  ( DispatchError(JsonError)
  , Dispatcher
  , GenericPendingRequests
  , PendingRequests
  , PendingSubmitTxRequests
  , RequestBody
  , WebsocketDispatch
  , mkWebsocketDispatch
  , newDispatcher
  , newPendingRequests
  )
import Ctl.Internal.QueryM.Dispatcher
  ( DispatchError(JsonError, FaultError, ListenerCancelled)
  , Dispatcher
  , GenericPendingRequests
  , PendingRequests
  , PendingSubmitTxRequests
  , RequestBody
  , WebsocketDispatch
  , dispatchErrorToError
  , mkWebsocketDispatch
  , newDispatcher
  , newPendingRequests
  ) as ExportDispatcher
import Ctl.Internal.QueryM.JsonRpc2
  ( OgmiosDecodeError
  , decodeOgmios
  , ogmiosDecodeErrorToError
  )
import Ctl.Internal.QueryM.JsonRpc2 as JsonRpc2
import Ctl.Internal.QueryM.Ogmios
  ( AdditionalUtxoSet
  , DelegationsAndRewardsR
  , HasTxR
  , MaybeMempoolTransaction
  , OgmiosProtocolParameters
  , PoolParametersR
  , ReleasedMempool
  , StakePoolsQueryArgument
  , TxHash
  )
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.QueryM.UniqueId (ListenerId)
import Ctl.Internal.ServerConfig
  ( Host
  , ServerConfig
  , defaultOgmiosWsConfig
  , mkHttpUrl
  , mkServerUrl
  , mkWsUrl
  ) as ExportServerConfig
import Ctl.Internal.ServerConfig (ServerConfig, mkWsUrl)
import Ctl.Internal.Service.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError, ClientDecodeJsonError)
  , ServiceError(ServiceOtherError)
  )
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.CborBytes (CborBytes)
import Ctl.Internal.Types.Chain as Chain
import Ctl.Internal.Types.Scripts (PlutusScript)
import Ctl.Internal.Types.SystemStart (SystemStart)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey, PrivateStakeKey)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, isRight)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(POST))
import Data.Log.Level (LogLevel(Error, Debug))
import Data.Log.Message (Message)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for_, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , ParAff
  , delay
  , launchAff_
  , makeAff
  , runAff_
  )
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref

-- | Cluster setup contains everything that is needed to run a `Contract` on
-- | a local cluster: paramters to connect to the services and private keys
-- | that are pre-funded with Ada on that cluster
type ClusterSetup =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , keys ::
      { payment :: PrivatePaymentKey
      , stake :: Maybe PrivateStakeKey
      }
  }

-- | `QueryConfig` contains a complete specification on how to initialize a
-- | `QueryM` environment.
-- | It includes:
-- | - server parameters for all the services
-- | - network ID
-- | - logging level
-- | - optional custom logger
type QueryConfig =
  { kupoConfig :: ServerConfig
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  }

-- | Reusable part of `QueryRuntime` that can be shared between many `QueryM`
-- |  instances running in parallel.
-- |
-- | Includes:
-- | - WebSocket connections
type QueryRuntime =
  { ogmiosWs :: OgmiosWebSocket
  }

-- | `QueryEnv` contains everything needed for `QueryM` to run.
type QueryEnv =
  { config :: QueryConfig
  , runtime :: QueryRuntime
  }

type QueryM = QueryMT Aff

type ParQueryM = QueryMT ParAff

newtype QueryMT (m :: Type -> Type) (a :: Type) =
  QueryMT (ReaderT QueryEnv m a)

derive instance Newtype (QueryMT m a) _
derive newtype instance Functor m => Functor (QueryMT m)
derive newtype instance Apply m => Apply (QueryMT m)
derive newtype instance Applicative m => Applicative (QueryMT m)
derive newtype instance Bind m => Bind (QueryMT m)
derive newtype instance Alt m => Alt (QueryMT m)
derive newtype instance Plus m => Plus (QueryMT m)
derive newtype instance Alternative m => Alternative (QueryMT m)
derive newtype instance Monad (QueryMT Aff)
derive newtype instance MonadEffect (QueryMT Aff)
derive newtype instance MonadAff (QueryMT Aff)
derive newtype instance
  ( Semigroup a
  , Apply m
  ) =>
  Semigroup (QueryMT m a)

derive newtype instance
  ( Monoid a
  , Applicative m
  ) =>
  Monoid (QueryMT m a)

derive newtype instance MonadThrow Error (QueryMT Aff)
derive newtype instance MonadError Error (QueryMT Aff)
derive newtype instance MonadRec (QueryMT Aff)
derive newtype instance MonadAsk QueryEnv (QueryMT Aff)
derive newtype instance MonadReader QueryEnv (QueryMT Aff)

instance MonadLogger (QueryMT Aff) where
  log msg = do
    config <- asks $ _.config
    let
      logFunction =
        config # _.customLogger >>> fromMaybe logWithLevel
    liftAff $ logFunction config.logLevel msg

-- Newtype deriving complains about overlapping instances, so we wrap and
-- unwrap manually
instance Parallel (QueryMT ParAff) (QueryMT Aff) where
  parallel :: QueryMT Aff ~> QueryMT ParAff
  parallel = wrap <<< parallel <<< unwrap
  sequential :: QueryMT ParAff ~> QueryMT Aff
  sequential = wrap <<< sequential <<< unwrap

getProtocolParametersAff
  :: OgmiosWebSocket
  -> (LogLevel -> String -> Effect Unit)
  -> Aff OgmiosProtocolParameters
getProtocolParametersAff ogmiosWs logger =
  mkOgmiosRequestAff ogmiosWs logger Ogmios.queryProtocolParametersCall
    _.getProtocolParameters
    unit

getSystemStartAff
  :: OgmiosWebSocket
  -> (LogLevel -> String -> Effect Unit)
  -> Aff SystemStart
getSystemStartAff ogmiosWs logger =
  unwrap <$> mkOgmiosRequestAff ogmiosWs logger Ogmios.querySystemStartCall
    _.systemStart
    unit

--------------------------------------------------------------------------------
-- Ogmios Local State Query Protocol
--------------------------------------------------------------------------------

getChainTip :: QueryM Chain.Tip
getChainTip = ogmiosChainTipToTip <$> mkOgmiosRequest Ogmios.queryChainTipCall
  _.chainTip
  unit
  where
  ogmiosChainTipToTip :: Ogmios.ChainTipQR -> Chain.Tip
  ogmiosChainTipToTip = case _ of
    Ogmios.CtChainOrigin _ -> Chain.TipAtGenesis
    Ogmios.CtChainPoint { slot, id } -> Chain.Tip $ wrap
      { slot, blockHeaderHash: wrap $ unwrap id }

--------------------------------------------------------------------------------
-- Ogmios Local Tx Submission Protocol
--------------------------------------------------------------------------------

submitTxOgmios :: TxHash -> CborBytes -> QueryM Ogmios.SubmitTxR
submitTxOgmios txHash tx = do
  ws <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  cfg <- asks _.config
  liftAff $ mkRequestAff listeners' ws (mkLogger cfg.logLevel cfg.customLogger)
    Ogmios.submitTxCall
    _.submit
    (txHash /\ tx)

evaluateTxOgmios
  :: CborBytes -> AdditionalUtxoSet -> QueryM Ogmios.TxEvaluationR
evaluateTxOgmios cbor additionalUtxos = do
  ws <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  cfg <- asks _.config
  liftAff $ mkRequestAff listeners' ws (mkLogger cfg.logLevel cfg.customLogger)
    Ogmios.evaluateTxCall
    _.evaluate
    (cbor /\ additionalUtxos)

--------------------------------------------------------------------------------
-- Ogmios Local Tx Monitor Protocol
--------------------------------------------------------------------------------

acquireMempoolSnapshotAff
  :: OgmiosWebSocket -> Logger -> Aff Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshotAff ogmiosWs logger =
  mkOgmiosRequestAff ogmiosWs logger Ogmios.acquireMempoolSnapshotCall
    _.acquireMempool
    unit

withMempoolSnapshot
  :: OgmiosWebSocket
  -> Logger
  -> (Maybe Ogmios.MempoolSnapshotAcquired -> Aff Unit)
  -> Effect Unit
withMempoolSnapshot ogmiosWs logger cont =
  flip runAff_ (acquireMempoolSnapshotAff ogmiosWs logger) $ case _ of
    Left err -> do
      logger Error $
        "Failed to acquire a mempool snapshot: Error: " <> show err
      launchAff_ (cont Nothing)
    Right mempoolSnapshot ->
      launchAff_ (cont $ Just mempoolSnapshot)

mempoolSnapshotHasTxAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> TxHash
  -> Aff Boolean
mempoolSnapshotHasTxAff ogmiosWs logger ms txh =
  unwrap <$> mkOgmiosRequestAff ogmiosWs logger
    (Ogmios.mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacityAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityAff ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger
    (Ogmios.mempoolSnapshotSizeAndCapacityCall ms)
    _.mempoolSizeAndCapacity -- todo: typo
    unit

releaseMempoolAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff ReleasedMempool
releaseMempoolAff ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger (Ogmios.releaseMempoolCall ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTxAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff (Maybe Ogmios.MempoolTransaction)
mempoolSnapshotNextTxAff ogmiosWs logger ms = unwrap <$>
  mkOgmiosRequestAff ogmiosWs logger (Ogmios.mempoolSnapshotNextTxCall ms)
    _.mempoolNextTx
    unit

acquireMempoolSnapshot
  :: QueryM Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshot =
  mkOgmiosRequest
    Ogmios.acquireMempoolSnapshotCall
    _.acquireMempool
    unit

mempoolSnapshotHasTx
  :: Ogmios.MempoolSnapshotAcquired
  -> TxHash
  -> QueryM Boolean
mempoolSnapshotHasTx ms txh =
  unwrap <$> mkOgmiosRequest
    (Ogmios.mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacity
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity ms =
  mkOgmiosRequest
    (Ogmios.mempoolSnapshotSizeAndCapacityCall ms)
    _.mempoolSizeAndCapacity
    unit

releaseMempool
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM Unit
releaseMempool ms =
  unit <$ mkOgmiosRequest
    (Ogmios.releaseMempoolCall ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTx
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM (Maybe Ogmios.MempoolTransaction)
mempoolSnapshotNextTx ms =
  unwrap <$> mkOgmiosRequest
    (Ogmios.mempoolSnapshotNextTxCall ms)
    _.mempoolNextTx
    unit

--------------------------------------------------------------------------------
-- Affjax
--------------------------------------------------------------------------------

-- Checks response status code and returns `ClientError` in case of failure,
-- otherwise attempts to decode the result.
--
-- This function solves the problem described there:
-- https://github.com/eviefp/purescript-affjax-errors
handleAffjaxResponse
  :: forall (result :: Type)
   . DecodeAeson result
  => Either Affjax.Error (Affjax.Response String)
  -> Either ClientError result
handleAffjaxResponse (Left affjaxError) =
  Left (ClientHttpError affjaxError)
handleAffjaxResponse
  (Right { status: Affjax.StatusCode.StatusCode statusCode, body })
  | statusCode < 200 || statusCode > 299 =
      Left $ ClientHttpResponseError (wrap statusCode) $ ServiceOtherError body
  | otherwise =
      body # lmap (ClientDecodeJsonError body)
        <<< (decodeAeson <=< parseJsonStringToAeson)

-- We can't use Affjax's typical `post`, since there will be a mismatch between
-- the media type header and the request body
postAeson :: Url -> Aeson -> Aff (Either Affjax.Error (Affjax.Response String))
postAeson url body = Affjax.request $ Affjax.defaultRequest
  { method = Left POST
  , content = Just $ Affjax.RequestBody.String $ stringifyAeson body
  , url = url
  , responseFormat = Affjax.ResponseFormat.string
  , headers = [ Affjax.RequestHeader.ContentType applicationJSON ]
  }

-- It's easier to just write the encoder here than provide an `EncodeJson`
-- instance (there are some brutal cyclical dependency issues trying to
-- write an instance in the `Types.*` modules)
scriptToAeson :: PlutusScript -> Aeson
scriptToAeson = encodeAeson <<< byteArrayToHex <<< fst <<< unwrap

--------------------------------------------------------------------------------
-- Type-safe `WebSocket`
--------------------------------------------------------------------------------

-- don't export this constructor
-- type-safe websocket which has automated req/res dispatch and websocket
-- failure handling
data WebSocket listeners = WebSocket JsWebSocket listeners
type OgmiosWebSocket = WebSocket OgmiosListeners

-- getter
underlyingWebSocket :: forall (a :: Type). WebSocket a -> JsWebSocket
underlyingWebSocket (WebSocket ws _) = ws

-- getter
listeners :: forall (listeners :: Type). WebSocket listeners -> listeners
listeners (WebSocket _ ls) = ls

--------------------------------------------------------------------------------
-- OgmiosWebSocket Setup and PrimOps
--------------------------------------------------------------------------------

type IsTxConfirmed = TxHash -> Aff Boolean

mkOgmiosWebSocketAff
  :: IsTxConfirmed
  -> Logger
  -> ServerConfig
  -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff isTxConfirmed logger serverConfig = do
  lens <- liftEffect $ mkOgmiosWebSocketLens logger isTxConfirmed
  makeAff $ mkServiceWebSocket lens (mkWsUrl serverConfig)

mkServiceWebSocket
  :: forall (listeners :: Type)
   . MkServiceWebSocketLens listeners
  -> Url
  -> (Either Error (WebSocket listeners) -> Effect Unit)
  -> Effect Canceler
mkServiceWebSocket lens url continue = do
  ws <- _mkWebSocket (lens.logger Debug) url
  let
    messageDispatch :: WebsocketDispatch
    messageDispatch = mkWebsocketDispatch lens.dispatcher

    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError :: String -> Effect Unit
    onFirstConnectionError errMessage = do
      _wsFinalize ws
      _wsClose ws
      lens.logger Error $
        "First connection to " <> lens.serviceName <> " WebSocket failed. "
          <> "Terminating. Error: "
          <> errMessage
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws onFirstConnectionError
  hasConnectedOnceRef <- Ref.new false
  _onWsConnect ws $ Ref.read hasConnectedOnceRef >>= case _ of
    true -> do
      lens.logger Debug $
        lens.serviceName <>
          " WebSocket connection re-established, resending pending requests..."
      lens.resendPendingRequests ws
    false -> do
      lens.logger Debug $ "Connection to " <> lens.serviceName <> " established"
      Ref.write true hasConnectedOnceRef
      _removeOnWsError ws firstConnectionErrorRef
      _onWsMessage ws (lens.logger Debug) $ defaultMessageListener lens.logger
        [ messageDispatch ]
      void $ _onWsError ws \err -> do
        lens.logger Debug $
          lens.serviceName <> " WebSocket error (" <> err <>
            "). Reconnecting..."
      continue $ Right (lens.typedWebSocket ws)
  pure $ Canceler $ \err -> liftEffect do
    _wsFinalize ws
    _wsClose ws
    continue $ Left $ err

--------------------------------------------------------------------------------
-- Resend pending `SubmitTx` requests
--------------------------------------------------------------------------------

-- | For each pending `SubmitTx` request, checks whether the transaction has
-- | been added to the mempool or has been included in a block before retrying
-- | the request.
resendPendingSubmitRequests
  :: OgmiosWebSocket
  -> IsTxConfirmed
  -> Logger
  -> (RequestBody -> Effect Unit)
  -> Dispatcher
  -> PendingSubmitTxRequests
  -> Effect Unit
resendPendingSubmitRequests
  ogmiosWs
  isTxConfirmed
  logger
  sendRequest
  dispatcher
  pr = do
  submitTxPendingRequests <- Ref.read pr
  unless (Map.isEmpty submitTxPendingRequests) do
    -- Acquiring a mempool snapshot should never fail and,
    -- after ws reconnection, should be instantaneous.
    withMempoolSnapshot ogmiosWs logger case _ of
      Nothing ->
        liftEffect $ traverse_ (sendRequest <<< fst) submitTxPendingRequests
      Just ms -> do
        -- A delay of 5 sec for transactions to be processed by the node
        -- and added to the mempool:
        delay (wrap 5000.0)
        let (pr' :: Array _) = Map.toUnfoldable submitTxPendingRequests
        for_ pr' \(listenerId /\ requestBody /\ txHash) ->
          handlePendingSubmitRequest ms listenerId requestBody txHash
  where
  log :: String -> Boolean -> TxHash -> Aff Unit
  log label value txHash =
    liftEffect $ logger Debug $
      label <> ": " <> show value <> " TxHash: " <> show txHash

  handlePendingSubmitRequest
    :: Ogmios.MempoolSnapshotAcquired
    -> ListenerId
    -> RequestBody
    -> TxHash
    -> Aff Unit
  handlePendingSubmitRequest ms listenerId requestBody txHash = do
    -- Check if the transaction was added to the mempool:
    txInMempool <- mempoolSnapshotHasTxAff ogmiosWs logger ms txHash
    log "Tx in the mempool" txInMempool txHash
    retrySubmitTx <-
      if txInMempool then pure false
      else do
        -- Check if the transaction was included in the block:
        txConfirmed <- isTxConfirmed txHash
        log "Tx confirmed" txConfirmed txHash
        unless txConfirmed $ liftEffect do
          sendRequest requestBody
        pure (not txConfirmed)
    -- Manually dispatch `SubmitTx` response if resending is not required:
    unless retrySubmitTx $ liftEffect do
      Ref.modify_ (Map.delete listenerId) pr
      dispatchMap <- Ref.read dispatcher
      Ref.modify_ (Map.delete listenerId) dispatcher
      Map.lookup listenerId dispatchMap #
        maybe (pure unit) (_ $ submitSuccessPartialResp)
    where
    submitSuccessPartialResp :: Aeson
    submitSuccessPartialResp =
      encodeAeson $ Ogmios.submitSuccessPartialResp txHash

--------------------------------------------------------------------------------
-- `MkServiceWebSocketLens` for ogmios
--------------------------------------------------------------------------------

type MkServiceWebSocketLens (listeners :: Type) =
  { serviceName :: String
  , dispatcher :: Dispatcher
  , logger :: Logger
  , typedWebSocket :: JsWebSocket -> WebSocket listeners
  , resendPendingRequests :: JsWebSocket -> Effect Unit
  }

mkOgmiosWebSocketLens
  :: Logger
  -> IsTxConfirmed
  -> Effect (MkServiceWebSocketLens OgmiosListeners)
mkOgmiosWebSocketLens logger isTxConfirmed = do
  dispatcher <- newDispatcher
  pendingRequests <- newPendingRequests
  pendingSubmitTxRequests <- newPendingRequests
  pure $
    let
      ogmiosWebSocket :: JsWebSocket -> OgmiosWebSocket
      ogmiosWebSocket ws = WebSocket ws
        { chainTip:
            mkListenerSet dispatcher pendingRequests
        , evaluate:
            mkListenerSet dispatcher pendingRequests
        , getProtocolParameters:
            mkListenerSet dispatcher pendingRequests
        , eraSummaries:
            mkListenerSet dispatcher pendingRequests
        , currentEpoch:
            mkListenerSet dispatcher pendingRequests
        , systemStart:
            mkListenerSet dispatcher pendingRequests
        , acquireMempool:
            mkListenerSet dispatcher pendingRequests
        , releaseMempool:
            mkListenerSet dispatcher pendingRequests
        , mempoolHasTx:
            mkListenerSet dispatcher pendingRequests
        , mempoolNextTx:
            mkListenerSet dispatcher pendingRequests
        , mempoolSizeAndCapacity:
            mkListenerSet dispatcher pendingRequests
        , submit:
            mkSubmitTxListenerSet dispatcher pendingSubmitTxRequests
        , stakePools:
            mkListenerSet dispatcher pendingRequests
        , delegationsAndRewards:
            mkListenerSet dispatcher pendingRequests
        }

      resendPendingRequests :: JsWebSocket -> Effect Unit
      resendPendingRequests ws = do
        let sendRequest = _wsSend ws (logger Debug)
        Ref.read pendingRequests >>= traverse_ sendRequest
        resendPendingSubmitRequests (ogmiosWebSocket ws) isTxConfirmed
          logger
          sendRequest
          dispatcher
          pendingSubmitTxRequests
    in
      { serviceName: "ogmios"
      , dispatcher
      , logger
      , typedWebSocket: ogmiosWebSocket
      , resendPendingRequests
      }

--------------------------------------------------------------------------------
-- ListenerSet
--------------------------------------------------------------------------------

type OgmiosListeners =
  { chainTip :: ListenerSet Unit Ogmios.ChainTipQR
  , submit :: SubmitTxListenerSet
  , evaluate ::
      ListenerSet (CborBytes /\ AdditionalUtxoSet) Ogmios.TxEvaluationR
  , getProtocolParameters :: ListenerSet Unit OgmiosProtocolParameters
  , eraSummaries :: ListenerSet Unit Ogmios.OgmiosEraSummaries
  , currentEpoch :: ListenerSet Unit Ogmios.CurrentEpoch
  , systemStart :: ListenerSet Unit Ogmios.OgmiosSystemStart
  , acquireMempool :: ListenerSet Unit Ogmios.MempoolSnapshotAcquired
  , releaseMempool :: ListenerSet Unit ReleasedMempool
  , mempoolHasTx :: ListenerSet TxHash HasTxR
  , mempoolNextTx :: ListenerSet Unit MaybeMempoolTransaction
  , mempoolSizeAndCapacity :: ListenerSet Unit Ogmios.MempoolSizeAndCapacity
  , stakePools :: ListenerSet StakePoolsQueryArgument PoolParametersR
  , delegationsAndRewards :: ListenerSet (Array String) DelegationsAndRewardsR
  }

-- convenience type for adding additional query types later
type ListenerSet (request :: Type) (response :: Type) =
  { addMessageListener ::
      ListenerId
      -> (Either OgmiosDecodeError response -> Effect Unit)
      -> Effect Unit
  , removeMessageListener :: ListenerId -> Effect Unit
  -- ^ Removes ID from dispatch map and pending requests queue.
  , addRequest :: ListenerId -> RequestBody /\ request -> Effect Unit
  -- ^ Saves request body until the request is fulfilled. The body is used
  --  to replay requests in case of a WebSocket failure.
  }

type SubmitTxListenerSet = ListenerSet (TxHash /\ CborBytes) Ogmios.SubmitTxR

mkAddMessageListener
  :: forall (response :: Type)
   . JsonRpc2.DecodeOgmios response
  => Dispatcher
  -> ( ListenerId
       -> (Either JsonRpc2.OgmiosDecodeError response -> Effect Unit)
       -> Effect Unit
     )
mkAddMessageListener dispatcher =
  \reflection handler ->
    flip Ref.modify_ dispatcher $
      Map.insert reflection
        (\aeson -> handler $ decodeOgmios aeson)

mkRemoveMessageListener
  :: forall (requestData :: Type)
   . Dispatcher
  -> GenericPendingRequests requestData
  -> (ListenerId -> Effect Unit)
mkRemoveMessageListener dispatcher pendingRequests =
  \reflection -> do
    Ref.modify_ (Map.delete reflection) dispatcher
    Ref.modify_ (Map.delete reflection) pendingRequests

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet
  :: forall (request :: Type) (response :: Type)
   . JsonRpc2.DecodeOgmios response
  => Dispatcher
  -> PendingRequests
  -> ListenerSet request response
mkListenerSet dispatcher pendingRequests =
  { addMessageListener:
      mkAddMessageListener dispatcher
  , removeMessageListener:
      mkRemoveMessageListener dispatcher pendingRequests
  , addRequest:
      \reflection (requestBody /\ _) ->
        Ref.modify_ (Map.insert reflection requestBody) pendingRequests
  }

mkSubmitTxListenerSet
  :: Dispatcher -> PendingSubmitTxRequests -> SubmitTxListenerSet
mkSubmitTxListenerSet dispatcher pendingRequests =
  { addMessageListener:
      mkAddMessageListener dispatcher
  , removeMessageListener:
      mkRemoveMessageListener dispatcher pendingRequests
  , addRequest:
      \reflection (requestBody /\ txHash /\ _) ->
        Ref.modify_ (Map.insert reflection (requestBody /\ txHash))
          pendingRequests
  }

-- | Builds an Ogmios request action using `QueryM`
mkOgmiosRequest
  :: forall (request :: Type) (response :: Type)
   . JsonRpc2.JsonRpc2Call request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkOgmiosRequest jsonRpc2Call getLs inp = do
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  websocket <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  mkRequest listeners' websocket jsonRpc2Call getLs inp

-- | Builds an Ogmios request action using `Aff`
mkOgmiosRequestAff
  :: forall (request :: Type) (response :: Type)
   . OgmiosWebSocket
  -> Logger
  -> JsonRpc2.JsonRpc2Call request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> Aff response
mkOgmiosRequestAff ogmiosWs = mkRequestAff
  (listeners ogmiosWs)
  (underlyingWebSocket ogmiosWs)

mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> JsonRpc2.JsonRpc2Call request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkRequest listeners' ws jsonRpc2Call getLs inp = do
  logger <- getLogger
  liftAff $ mkRequestAff listeners' ws logger jsonRpc2Call getLs inp

getLogger :: QueryM Logger
getLogger = do
  logLevel <- asks $ _.config >>> _.logLevel
  mbCustomLogger <- asks $ _.config >>> _.customLogger
  pure $ mkLogger logLevel mbCustomLogger

mkRequestAff
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> Logger
  -> JsonRpc2.JsonRpc2Call request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> Aff response
mkRequestAff listeners' webSocket logger jsonRpc2Call getLs input = do
  { body, id } <-
    liftEffect $ JsonRpc2.buildRequest jsonRpc2Call input
  let
    respLs :: ListenerSet request response
    respLs = getLs listeners'

    sBody :: RequestBody
    sBody = stringifyAeson body

    affFunc :: (Either Error response -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      _ <- respLs.addMessageListener id
        ( \res -> do
            respLs.removeMessageListener id
            cont $ lmap ogmiosDecodeErrorToError res
        )
      respLs.addRequest id (sBody /\ input)
      _wsSend webSocket (logger Debug) sBody
      -- Uncomment this code fragment to test `SubmitTx` request resend logic:
      -- let method = aesonObject (flip getFieldOptional "methodname") body
      -- when (method == Right (Just "SubmitTx")) do
      --   _wsReconnect webSocket
      pure $ Canceler $ \err -> do
        liftEffect $ respLs.removeMessageListener id
        liftEffect $ throwError $ err
  makeAff affFunc

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: JsonDecodeError
defaultErr = TypeMismatch "default error"

defaultMessageListener
  :: Logger
  -> Array WebsocketDispatch
  -> String
  -> Effect Unit
defaultMessageListener logger dispatchArray msg = do
  aeson <- liftEither $ lmap (const $ error "Unable to parse response") $
    parseJsonStringToAeson msg
  -- here, we need to fold the input over the array of functions until we get
  -- a success, then execute the effect.
  -- using a fold instead of a traverse allows us to skip a bunch of execution
  eAction :: Either DispatchError (Effect Unit) <- foldl
    (messageFoldF aeson)
    (pure $ Left $ JsonError defaultErr)
    dispatchArray
  either
    -- we expect a lot of parse errors, some messages (could?) fall through completely
    ( \err ->
        unless
          ( case err of
              JsonError jsonErr -> jsonErr == defaultErr
              _ -> false
          )
          do
            logger Error $
              "unexpected error on input: " <> msg
                <> " Error:"
                <> show err
    )
    identity
    eAction

messageFoldF
  :: Aeson
  -> Effect (Either DispatchError (Effect Unit))
  -> (Aeson -> (Effect (Either DispatchError (Effect Unit))))
  -> Effect (Either DispatchError (Effect Unit))
messageFoldF msg acc' func = do
  acc <- acc'
  if isRight acc then acc' else func msg
