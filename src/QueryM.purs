-- | TODO docstring
module QueryM
  ( ClientError
      ( ClientHttpError
      , ClientHttpResponseError
      , ClientDecodeJsonError
      , ClientEncodingError
      , ClientOtherError
      )
  , DatumCacheListeners
  , DatumCacheWebSocket
  , DefaultQueryEnv
  , DispatchError(JsError, JsonError, FaultError, ListenerCancelled)
  , DispatchIdMap
  , ListenerSet
  , Logger
  , OgmiosListeners
  , OgmiosWebSocket
  , PendingRequests
  , QueryConfig
  , QueryM
  , QueryMExtended(QueryMExtended)
  , QueryEnv
  , QueryRuntime
  , RequestBody
  , WebSocket(WebSocket)
  , allowError
  , applyArgs
  , evaluateTxOgmios
  , getChainTip
  , getDatumByHash
  , getDatumsByHashes
  , getLogger
  , getProtocolParametersAff
  , getWalletAddresses
  , liftQueryM
  , listeners
  , postAeson
  , mkDatumCacheWebSocketAff
  , mkDatumCacheRequest
  , mkLogger
  , queryDispatch
  , defaultMessageListener
  , mkListenerSet
  , mkOgmiosRequest
  , mkOgmiosRequestAff
  , mkOgmiosWebSocketAff
  , mkQueryRuntime
  , mkRequest
  , mkRequestAff
  , module ServerConfig
  , ownPaymentPubKeyHashes
  , ownPubKeyHashes
  , ownStakePubKeyHash
  , runQueryM
  , runQueryMWithSettings
  , runQueryMInRuntime
  , signTransaction
  , scriptToAeson
  , stopQueryRuntime
  , submitTxOgmios
  , underlyingWebSocket
  , withQueryRuntime
  , callCip30Wallet
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
import Affjax (Error, Response, defaultRequest, printError, request) as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Affjax.StatusCode as Affjax.StatusCode
import Cardano.Types.Transaction (_witnessSet)
import Cardano.Types.Transaction as Transaction
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT, asks, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel (parallel, sequential)
import Data.Array (head, singleton) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, isRight)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(POST))
import Data.JSDate (now)
import Data.Lens ((<>~))
import Data.Log.Level (LogLevel(Error, Debug))
import Data.Log.Message (Message)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , delay
  , finally
  , launchAff_
  , makeAff
  , runAff_
  , supervise
  )
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as Object
import Helpers (logString, logWithLevel)
import JsWebSocket
  ( JsWebSocket
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _removeOnWsError
  , _wsClose
  , _wsReconnect
  , _wsSend
  )
import QueryM.DatumCacheWsp (GetDatumByHashR, GetDatumsByHashesR, GetTxByHashR)
import QueryM.DatumCacheWsp as DcWsp
import QueryM.JsonWsp (parseJsonWspResponseId)
import QueryM.JsonWsp as JsonWsp
import QueryM.Ogmios (TxHash)
import QueryM.Ogmios as Ogmios
import QueryM.ServerConfig
  ( Host
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , mkHttpUrl
  , mkOgmiosDatumCacheWsUrl
  , mkServerUrl
  , mkWsUrl
  ) as ServerConfig
import QueryM.ServerConfig
  ( ServerConfig
  , mkHttpUrl
  , mkOgmiosDatumCacheWsUrl
  , mkWsUrl
  )
import QueryM.UniqueId (ListenerId)
import Serialization (toBytes) as Serialization
import Serialization.Address
  ( Address
  , NetworkId
  , addressPaymentCred
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , stakeCredentialToKeyHash
  )
import Serialization.PlutusData (convertPlutusData) as Serialization
import Types.ByteArray (byteArrayToHex)
import Types.CborBytes (CborBytes)
import Types.Chain as Chain
import Types.Datum (DataHash, Datum)
import Types.MultiMap (MultiMap)
import Types.MultiMap as MultiMap
import Types.PlutusData (PlutusData)
import Types.PubKeyHash (PaymentPubKeyHash, PubKeyHash, StakePubKeyHash)
import Types.Scripts (PlutusScript)
import Types.Transaction (TransactionInput)
import Types.UsedTxOuts (newUsedTxOuts, UsedTxOuts)
import Untagged.Union (asOneOf)
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Flint, Nami, Lode, Eternl, KeyWallet)
  , mkGeroWalletAff
  , mkFlintWalletAff
  , mkEternlWalletAff
  , mkKeyWallet
  , mkNamiWalletAff
  , mkLodeWalletAff
  )
import Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Wallet.Spec
  ( WalletSpec
      ( UseKeys
      , ConnectToGero
      , ConnectToNami
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      )
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  )

-- This module defines an Aff interface for Ogmios Websocket Queries
-- Since WebSockets do not define a mechanism for linking request/response
-- Or for verifying that the connection is live, those concerns are addressed
-- here

-- | `QueryConfig` contains a complete specification on how to initialize a
-- | `QueryM` environment.
-- | It includes:
-- | - server parameters for all the services
-- | - network ID
-- | - logging level
-- | - wallet setup instructions
-- | - optional custom logger
type QueryConfig =
  { ctlServerConfig :: Maybe ServerConfig
  , ogmiosConfig :: ServerConfig
  , datumCacheConfig :: ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (Message -> Aff Unit)
  , suppressLogs :: Boolean
  }

-- | Reusable part of `QueryRuntime` that can be shared between many `QueryM`
-- |  instances running in parallel.
-- |
-- | Includes:
-- | - WebSocket connections
-- | - A wallet connection
-- | - A data structure to keep UTxOs that has already been spent
-- | - Current protocol parameters
type QueryRuntime =
  { ogmiosWs :: OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , wallet :: Maybe Wallet
  , usedTxOuts :: UsedTxOuts
  , pparams :: Ogmios.ProtocolParameters
  }

-- | `QueryEnv` contains everything needed for `QueryM` to run.
type QueryEnv (r :: Row Type) =
  { config :: QueryConfig
  , runtime :: QueryRuntime
  , extraConfig :: { | r }
  }

type DefaultQueryEnv = QueryEnv ()

type QueryM (a :: Type) = QueryMExtended () a

newtype QueryMExtended (r :: Row Type) (a :: Type) = QueryMExtended
  (ReaderT (QueryEnv r) Aff a)

derive instance Newtype (QueryMExtended r a) _
derive newtype instance Functor (QueryMExtended r)
derive newtype instance Apply (QueryMExtended r)
derive newtype instance Applicative (QueryMExtended r)
derive newtype instance Bind (QueryMExtended r)
derive newtype instance Monad (QueryMExtended r)
derive newtype instance MonadEffect (QueryMExtended r)
derive newtype instance MonadAff (QueryMExtended r)
derive newtype instance Semigroup a => Semigroup (QueryMExtended r a)
derive newtype instance Monoid a => Monoid (QueryMExtended r a)
derive newtype instance MonadThrow Error (QueryMExtended r)
derive newtype instance MonadError Error (QueryMExtended r)
derive newtype instance MonadRec (QueryMExtended r)
derive newtype instance MonadAsk (QueryEnv r) (QueryMExtended r)
derive newtype instance MonadReader (QueryEnv r) (QueryMExtended r)

instance MonadLogger (QueryMExtended r) where
  log msg = do
    config <- asks $ _.config
    let
      logFunction =
        config # _.customLogger >>> fromMaybe (logWithLevel config.logLevel)
    liftAff $ logFunction msg

liftQueryM :: forall (r :: Row Type) (a :: Type). QueryM a -> QueryMExtended r a
liftQueryM = unwrap >>> withReaderT toDefaultQueryEnv >>> wrap
  where
  toDefaultQueryEnv :: QueryEnv r -> DefaultQueryEnv
  toDefaultQueryEnv c = c { extraConfig = {} }

-- | Constructs and finalizes a contract environment that is usable inside a
-- | bracket callback.
-- | Make sure that `Aff` action does not end before all contracts that use the
-- | runtime terminate. Otherwise `WebSocket`s will be closed too early.
withQueryRuntime
  :: forall a
   . QueryConfig
  -> (QueryRuntime -> Aff a)
  -> Aff a
withQueryRuntime config action = do
  runtime <- mkQueryRuntime config
  supervise (action runtime) `flip finally` do
    liftEffect $ stopQueryRuntime runtime

-- | Close the websockets in `QueryRuntime`, effectively making it unusable
stopQueryRuntime
  :: QueryRuntime
  -> Effect Unit
stopQueryRuntime runtime = do
  _wsClose $ underlyingWebSocket runtime.ogmiosWs
  _wsClose $ underlyingWebSocket runtime.datumCacheWs

-- | Used in `mkQueryRuntime` only
data QueryRuntimeModel = QueryRuntimeModel
  (OgmiosWebSocket /\ DatumCacheWebSocket /\ Ogmios.ProtocolParameters)
  (Maybe Wallet)

mkQueryRuntime
  :: QueryConfig
  -> Aff QueryRuntime
mkQueryRuntime config = do
  usedTxOuts <- newUsedTxOuts
  QueryRuntimeModel (ogmiosWs /\ datumCacheWs /\ pparams) wallet <- sequential $
    QueryRuntimeModel
      <$> parallel do
        datumCacheWs <-
          mkDatumCacheWebSocketAff logger config.datumCacheConfig
        ogmiosWs <-
          mkOgmiosWebSocketAff datumCacheWs logger config.ogmiosConfig
        pparams <- getProtocolParametersAff ogmiosWs logger
        pure $ ogmiosWs /\ datumCacheWs /\ pparams
      <*> parallel (for config.walletSpec mkWalletBySpec)
  pure
    { ogmiosWs
    , datumCacheWs
    , wallet
    , usedTxOuts
    , pparams
    }
  where
  logger = mkLogger config.logLevel config.customLogger

mkWalletBySpec :: WalletSpec -> Aff Wallet
mkWalletBySpec = case _ of
  UseKeys paymentKeySpec mbStakeKeySpec -> do
    privatePaymentKey <- case paymentKeySpec of
      PrivatePaymentKeyFile filePath ->
        privatePaymentKeyFromFile filePath
      PrivatePaymentKeyValue key -> pure key
    mbPrivateStakeKey <- for mbStakeKeySpec case _ of
      PrivateStakeKeyFile filePath -> privateStakeKeyFromFile filePath
      PrivateStakeKeyValue key -> pure key
    pure $ mkKeyWallet privatePaymentKey mbPrivateStakeKey
  ConnectToNami -> mkNamiWalletAff
  ConnectToGero -> mkGeroWalletAff
  ConnectToFlint -> mkFlintWalletAff
  ConnectToEternl -> mkEternlWalletAff
  ConnectToLode -> mkLodeWalletAff

runQueryM :: forall (a :: Type). QueryConfig -> QueryM a -> Aff a
runQueryM config action = do
  withQueryRuntime config \runtime ->
    runQueryMInRuntime config runtime action

runQueryMWithSettings
  :: forall (r :: Row Type) (a :: Type)
   . QueryEnv r
  -> QueryM a
  -> Aff a
runQueryMWithSettings settings action = do
  runQueryMInRuntime settings.config settings.runtime action

runQueryMInRuntime
  :: forall (a :: Type)
   . QueryConfig
  -> QueryRuntime
  -> QueryM a
  -> Aff a
runQueryMInRuntime config runtime = do
  flip runReaderT { config, runtime, extraConfig: {} } <<< unwrap

getProtocolParametersAff
  :: OgmiosWebSocket
  -> (LogLevel -> String -> Effect Unit)
  -> Aff Ogmios.ProtocolParameters
getProtocolParametersAff ogmiosWs logger =
  mkOgmiosRequestAff ogmiosWs logger Ogmios.queryProtocolParametersCall
    _.getProtocolParameters
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
    Ogmios.CtChainPoint { slot, hash } -> Chain.Tip $ wrap
      { slot, blockHeaderHash: wrap $ unwrap hash }

--------------------------------------------------------------------------------
-- Ogmios Local Tx Submission Protocol
--------------------------------------------------------------------------------

submitTxOgmios :: TxHash -> CborBytes -> QueryM Ogmios.SubmitTxR
submitTxOgmios txHash tx = do
  ws <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  cfg <- asks _.config
  let inp = RequestInputToStoreInPendingRequests (txHash /\ tx)
  liftAff $ mkRequestAff' listeners' ws (mkLogger cfg.logLevel cfg.customLogger)
    Ogmios.submitTxCall
    _.submit
    inp

evaluateTxOgmios :: CborBytes -> QueryM Ogmios.TxEvaluationR
evaluateTxOgmios = mkOgmiosRequest Ogmios.evaluateTxCall _.evaluate

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
mempoolSnapshotHasTxAff ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger (Ogmios.mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx

--------------------------------------------------------------------------------
-- Datum Cache Queries
--------------------------------------------------------------------------------

getDatumByHash :: DataHash -> QueryM (Maybe Datum)
getDatumByHash hash = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumByHashCall _.getDatumByHash hash

getDatumsByHashes :: Array DataHash -> QueryM (Map DataHash Datum)
getDatumsByHashes hashes = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumsByHashesCall _.getDatumsByHashes hashes

checkTxByHashAff :: DatumCacheWebSocket -> Logger -> TxHash -> Aff Boolean
checkTxByHashAff datumCacheWs logger =
  mkDatumCacheRequestAff datumCacheWs logger DcWsp.getTxByHash _.getTxByHash
    >>> map (unwrap >>> isJust)

allowError
  :: forall (a :: Type). (Either Error a -> Effect Unit) -> a -> Effect Unit
allowError func = func <<< Right

--------------------------------------------------------------------------------
-- Wallet
--------------------------------------------------------------------------------

getWalletAddresses :: QueryM (Maybe (Array Address))
getWalletAddresses = do
  networkId <- asks $ _.config >>> _.networkId
  withMWalletAff case _ of
    Nami wallet -> callCip30Wallet wallet _.getWalletAddresses
    Gero wallet -> callCip30Wallet wallet _.getWalletAddresses
    Flint wallet -> callCip30Wallet wallet _.getWalletAddresses
    Lode wallet -> callCip30Wallet wallet _.getWalletAddresses
    Eternl wallet -> callCip30Wallet wallet _.getWalletAddresses
    KeyWallet kw -> (Just <<< Array.singleton) <$> (unwrap kw).address networkId

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = withMWalletAff case _ of
  Nami nami -> callCip30Wallet nami \nw -> flip nw.signTx tx
  Gero gero -> callCip30Wallet gero \nw -> flip nw.signTx tx
  Flint flint -> callCip30Wallet flint \nw -> flip nw.signTx tx
  Eternl eternl -> callCip30Wallet eternl \nw -> flip nw.signTx tx
  Lode lode -> callCip30Wallet lode \nw -> flip nw.signTx tx
  KeyWallet kw -> do
    witnessSet <- (unwrap kw).signTx tx
    pure $ Just (tx # _witnessSet <>~ witnessSet)

ownPubKeyHashes :: QueryM (Maybe (Array PubKeyHash))
ownPubKeyHashes = do
  mbAddress <- getWalletAddresses
  pure $
    map wrap <$>
      (mbAddress >>= traverse (addressPaymentCred >=> stakeCredentialToKeyHash))

ownPaymentPubKeyHashes :: QueryM (Maybe (Array PaymentPubKeyHash))
ownPaymentPubKeyHashes = (map <<< map) wrap <$> ownPubKeyHashes

-- TODO: change to array of StakePubKeyHash
ownStakePubKeyHash :: QueryM (Maybe StakePubKeyHash)
ownStakePubKeyHash = do
  mbAddress <- getWalletAddresses <#> (_ >>= Array.head)
  pure do
    baseAddress <- mbAddress >>= baseAddressFromAddress
    wrap <<< wrap <$> stakeCredentialToKeyHash
      (baseAddressDelegationCred baseAddress)

withMWalletAff
  :: forall (a :: Type). (Wallet -> Aff (Maybe a)) -> QueryM (Maybe a)
withMWalletAff act = asks (_.runtime >>> _.wallet) >>= maybe (pure Nothing)
  (liftAff <<< act)

callCip30Wallet
  :: forall (a :: Type)
   . Cip30Wallet
  -> (Cip30Wallet -> (Cip30Connection -> Aff a))
  -> Aff a
callCip30Wallet wallet act = act wallet wallet.connection

data ClientError
  = ClientHttpError Affjax.Error
  | ClientHttpResponseError String
  | ClientDecodeJsonError JsonDecodeError
  | ClientEncodingError String
  | ClientOtherError String

-- No Show instance of Affjax.Error
instance Show ClientError where
  show (ClientHttpError err) =
    "(ClientHttpError "
      <> Affjax.printError err
      <> ")"
  show (ClientHttpResponseError err) =
    "(ClientHttpResponseError "
      <> show err
      <> ")"
  show (ClientDecodeJsonError err) =
    "(ClientDecodeJsonError "
      <> show err
      <> ")"
  show (ClientEncodingError err) =
    "(ClientEncodingError "
      <> err
      <> ")"
  show (ClientOtherError err) =
    "(ClientEncodingError "
      <> err
      <> ")"

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgs
  :: forall (a :: Type)
   . Newtype a PlutusScript
  => DecodeAeson a
  => a
  -> Array PlutusData
  -> QueryM (Either ClientError a)
applyArgs script args = asks (_.ctlServerConfig <<< _.config) >>= case _ of
  Nothing ->
    pure
      $ Left
      $
        ClientOtherError
          "The `ctl-server` service is required to call `applyArgs`. Please \
          \provide a `Just` value in `ConfigParams.ctlServerConfig` and make \
          \sure that the `ctl-server` service is running and available at the \
          \provided host and port. The `ctl-server` packages can be obtained \
          \from `overlays.ctl-server` defined in CTL's flake. Please see \
          \`doc/runtime.md` in the CTL repository for more information"
  Just config -> case traverse plutusDataToAeson args of
    Nothing -> pure $ Left $ ClientEncodingError "Failed to convert script args"
    Just ps -> do
      let
        url :: String
        url = mkHttpUrl config <> "/apply-args"

        reqBody :: Aeson
        reqBody = encodeAeson
          $ Object.fromFoldable
              [ "script" /\ scriptToAeson (unwrap script)
              , "args" /\ encodeAeson ps
              ]

      liftAff (postAeson url reqBody)
        <#> map wrap <<< handleAffjaxResponse
    where
    plutusDataToAeson :: PlutusData -> Maybe Aeson
    plutusDataToAeson =
      map
        ( encodeAeson
            <<< byteArrayToHex
            <<< Serialization.toBytes
            <<< asOneOf
        )
        <<< Serialization.convertPlutusData

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
      Left (ClientHttpResponseError body)
  | otherwise =
      body # lmap ClientDecodeJsonError
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
scriptToAeson = encodeAeson <<< byteArrayToHex <<< unwrap

--------------------------------------------------------------------------------
-- OgmiosWebSocket Setup and PrimOps
--------------------------------------------------------------------------------

-- don't export this constructor
-- type-safe websocket which has automated req/res dispatch and websocket
-- failure handling
data WebSocket listeners = WebSocket JsWebSocket listeners
type OgmiosWebSocket = WebSocket OgmiosListeners
type DatumCacheWebSocket = WebSocket DatumCacheListeners

-- smart-constructor for OgmiosWebSocket in Aff Context
-- (prevents sending messages before the websocket opens, etc)
mkOgmiosWebSocket'
  :: DatumCacheWebSocket
  -> Logger
  -> ServerConfig
  -> (Either Error OgmiosWebSocket -> Effect Unit)
  -> Effect Canceler
mkOgmiosWebSocket' datumCacheWs logger serverCfg continue = do
  utxoDispatchMap <- createMutableDispatch
  utxosAtDispatchMap <- createMutableDispatch
  chainTipDispatchMap <- createMutableDispatch
  evaluateTxDispatchMap <- createMutableDispatch
  getProtocolParametersDispatchMap <- createMutableDispatch
  submitDispatchMap <- createMutableDispatch
  eraSummariesDispatchMap <- createMutableDispatch
  currentEpochDispatchMap <- createMutableDispatch
  systemStartDispatchMap <- createMutableDispatch
  acquireMempoolDispatchMap <- createMutableDispatch
  mempoolHasTxDispatchMap <- createMutableDispatch
  utxoPendingRequests <- createPendingRequests
  utxosAtPendingRequests <- createPendingRequests
  chainTipPendingRequests <- createPendingRequests
  evaluateTxPendingRequests <- createPendingRequests
  getProtocolParametersPendingRequests <- createPendingRequests
  submitPendingRequests <- createPendingRequests
  eraSummariesPendingRequests <- createPendingRequests
  currentEpochPendingRequests <- createPendingRequests
  systemStartPendingRequests <- createPendingRequests
  acquireMempoolPendingRequests <- createPendingRequests
  mempoolHasTxPendingRequests <- createPendingRequests
  let
    messageDispatch = ogmiosMessageDispatch
      { utxoDispatchMap
      , utxosAtDispatchMap
      , chainTipDispatchMap
      , evaluateTxDispatchMap
      , getProtocolParametersDispatchMap
      , submitDispatchMap
      , eraSummariesDispatchMap
      , currentEpochDispatchMap
      , systemStartDispatchMap
      , acquireMempoolDispatchMap
      , mempoolHasTxDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let

    ogmiosWs :: OgmiosWebSocket
    ogmiosWs = WebSocket ws
      { utxo:
          mkListenerSet utxoDispatchMap utxoPendingRequests
      , utxosAt:
          mkListenerSet utxosAtDispatchMap utxosAtPendingRequests
      , chainTip:
          mkListenerSet chainTipDispatchMap chainTipPendingRequests
      , evaluate:
          mkListenerSet evaluateTxDispatchMap evaluateTxPendingRequests
      , getProtocolParameters:
          mkListenerSet getProtocolParametersDispatchMap
            getProtocolParametersPendingRequests
      , submit:
          mkListenerSet submitDispatchMap submitPendingRequests
      , eraSummaries:
          mkListenerSet eraSummariesDispatchMap eraSummariesPendingRequests
      , currentEpoch:
          mkListenerSet currentEpochDispatchMap currentEpochPendingRequests
      , systemStart:
          mkListenerSet systemStartDispatchMap systemStartPendingRequests
      , acquireMempool:
          mkListenerSet acquireMempoolDispatchMap acquireMempoolPendingRequests
      , mempoolHasTx:
          mkListenerSet mempoolHasTxDispatchMap mempoolHasTxPendingRequests
      }

    sendRequest :: forall (req :: Type). RequestBody /\ req -> Effect Unit
    sendRequest = _wsSend ws (logger Debug) <<< Tuple.fst

    resendPendingRequests = do
      Ref.read utxoPendingRequests >>= traverse_ sendRequest
      Ref.read utxosAtPendingRequests >>= traverse_ sendRequest
      Ref.read chainTipPendingRequests >>= traverse_ sendRequest
      Ref.read evaluateTxPendingRequests >>= traverse_ sendRequest
      Ref.read getProtocolParametersPendingRequests >>= traverse_ sendRequest
      Ref.read eraSummariesPendingRequests >>= traverse_ sendRequest
      Ref.read currentEpochPendingRequests >>= traverse_ sendRequest
      Ref.read systemStartPendingRequests >>= traverse_ sendRequest

      logger Debug "Resent all pending requests"

      Ref.write MultiMap.empty acquireMempoolDispatchMap
      Ref.write Map.empty acquireMempoolPendingRequests
      Ref.write MultiMap.empty mempoolHasTxDispatchMap
      Ref.write Map.empty mempoolHasTxPendingRequests

      resendPendingSubmitRequests ogmiosWs datumCacheWs logger sendRequest
        submitDispatchMap
        submitPendingRequests

    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError errMessage = do
      _wsClose ws
      logger Error $
        "First connection to Ogmios WebSocket failed. Terminating. Error: " <>
          errMessage
      _wsClose ws
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws onFirstConnectionError
  hasConnectedOnceRef <- Ref.new false
  _onWsConnect ws $ Ref.read hasConnectedOnceRef >>= case _ of
    true -> do
      logger Debug
        "Ogmios WS connection re-established, resending pending requests..."
      resendPendingRequests
      logger Debug "Resent all pending requests"
    false -> do
      logger Debug "Ogmios Connection established"
      Ref.write true hasConnectedOnceRef
      _removeOnWsError ws firstConnectionErrorRef
      _onWsMessage ws (logger Debug) $ defaultMessageListener logger
        messageDispatch
      void $ _onWsError ws \err -> do
        logger Debug $
          "Ogmios WebSocket error (" <> err <> "). Reconnecting..."
        launchAff_ do
          delay (wrap 500.0)
          liftEffect $ _wsReconnect ws
      continue (Right ogmiosWs)
  pure $ Canceler $ \err -> liftEffect do
    _wsClose ws
    continue $ Left $ err

-- | For all pending `SubmitTx` requests checks if a transaction was added
-- | to the mempool or included in the block before retrying the request.
resendPendingSubmitRequests
  :: OgmiosWebSocket
  -> DatumCacheWebSocket
  -> Logger
  -> (forall (inp :: Type). RequestBody /\ inp -> Effect Unit)
  -> DispatchIdMap Ogmios.SubmitTxR
  -> PendingRequests (TxHash /\ CborBytes)
  -> Effect Unit
resendPendingSubmitRequests ogmiosWs datumCacheWs logger sendRequest dim pr = do
  submitPendingRequests <- Ref.read pr
  unless (Map.isEmpty submitPendingRequests) do
    -- Acquiring a mempool snapshot should never fail and,
    -- after ws reconnection, should be instantaneous.
    withMempoolSnapshot ogmiosWs logger case _ of
      Nothing ->
        liftEffect $ traverse_ sendRequest submitPendingRequests
      Just ms -> do
        -- A delay of 5 sec for transactions to be processed by the node
        -- and added to the mempool:
        delay (wrap 5000.0)
        let (pr' :: Array _) = Map.toUnfoldable submitPendingRequests
        for_ pr' \(listenerId /\ requestBody /\ requestInput) ->
          case requestInput of
            Nothing ->
              liftEffect $ sendRequest (requestBody /\ unit)
            Just (txHash /\ _) -> do
              handlePendingSubmitRequest ms listenerId requestBody txHash
  where
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
        txConfirmed <- checkTxByHashAff datumCacheWs logger txHash
        log "Tx confirmed" txConfirmed txHash
        unless txConfirmed $ liftEffect do
          sendRequest (requestBody /\ unit)
        pure (not txConfirmed)
    -- Manually dispatch `SubmitTx` response if resending is not required:
    unless retrySubmitTx $ liftEffect do
      Ref.modify_ (Map.delete listenerId) pr
      dispatchMap <- Ref.read dim
      Ref.modify_ (MultiMap.delete listenerId) dim
      MultiMap.lookup listenerId dispatchMap #
        maybe (pure unit) (_ $ Right $ Ogmios.SubmitTxSuccess txHash)

  log :: String -> Boolean -> TxHash -> Aff Unit
  log label value txHash =
    liftEffect $ logger Debug $
      label <> ": " <> show value <> " TxHash: " <> show txHash

mkDatumCacheWebSocket'
  :: Logger
  -> ServerConfig
  -> (Either Error DatumCacheWebSocket -> Effect Unit)
  -> Effect Canceler
mkDatumCacheWebSocket' logger serverCfg continue = do
  getDatumByHashDispatchMap <- createMutableDispatch
  getDatumsByHashesDispatchMap <- createMutableDispatch
  getTxByHashDispatchMap <- createMutableDispatch
  getDatumByHashPendingRequests <- createPendingRequests
  getDatumsByHashesPendingRequests <- createPendingRequests
  getTxByHashPendingRequests <- createPendingRequests
  let
    messageDispatch = datumCacheMessageDispatch
      { getDatumByHashDispatchMap
      , getDatumsByHashesDispatchMap
      , getTxByHashDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkOgmiosDatumCacheWsUrl serverCfg
  let
    sendRequest :: forall (inp :: Type). RequestBody /\ inp -> Effect Unit
    sendRequest = _wsSend ws (logger Debug) <<< Tuple.fst

    resendPendingRequests = do
      Ref.read getDatumByHashPendingRequests >>= traverse_ sendRequest
      Ref.read getDatumsByHashesPendingRequests >>= traverse_ sendRequest
      Ref.read getTxByHashPendingRequests >>= traverse_ sendRequest
    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError errMessage = do
      _wsClose ws
      logger Error $
        "First connection to Ogmios Datum Cache WebSocket failed. "
          <> "Terminating. Error: "
          <> errMessage
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws onFirstConnectionError
  hasConnectedOnceRef <- Ref.new false
  _onWsConnect ws $ Ref.read hasConnectedOnceRef >>= case _ of
    true -> do
      logger Debug $
        "Ogmios Datum Cache WS connection re-established, resending " <>
          "pending requests..."
      resendPendingRequests
    false -> do
      logger Debug "Ogmios Datum Cache Connection established"
      Ref.write true hasConnectedOnceRef
      _removeOnWsError ws firstConnectionErrorRef
      _onWsMessage ws (logger Debug) $ defaultMessageListener logger
        messageDispatch
      void $ _onWsError ws \err -> do
        logger Debug $
          "Ogmios Datum Cache WebSocket error (" <> err <>
            "). Reconnecting..."
        launchAff_ do
          delay (wrap 500.0)
          liftEffect $ _wsReconnect ws
      continue $ Right $ WebSocket ws
        { getDatumByHash: mkListenerSet getDatumByHashDispatchMap
            getDatumByHashPendingRequests
        , getDatumsByHashes: mkListenerSet getDatumsByHashesDispatchMap
            getDatumsByHashesPendingRequests
        , getTxByHash: mkListenerSet getTxByHashDispatchMap
            getTxByHashPendingRequests
        }
  pure $ Canceler $ \err -> liftEffect do
    _wsClose ws
    continue $ Left $ err

mkDatumCacheWebSocketAff
  :: Logger
  -> ServerConfig
  -> Aff DatumCacheWebSocket
mkDatumCacheWebSocketAff logger = makeAff <<< mkDatumCacheWebSocket' logger

mkOgmiosWebSocketAff
  :: DatumCacheWebSocket -> Logger -> ServerConfig -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff datumCacheWs logger =
  makeAff <<< mkOgmiosWebSocket' datumCacheWs logger

-- getter
underlyingWebSocket :: forall (a :: Type). WebSocket a -> JsWebSocket
underlyingWebSocket (WebSocket ws _) = ws

-- getter
listeners :: forall (listeners :: Type). WebSocket listeners -> listeners
listeners (WebSocket _ ls) = ls

type PendingRequests (request :: Type) =
  Ref (Map ListenerId (RequestBody /\ Maybe request))

data RequestInput (request :: Type)
  = RequestInput request
  | RequestInputToStoreInPendingRequests request

getRequestInput :: forall (request :: Type). RequestInput request -> request
getRequestInput (RequestInput inp) = inp
getRequestInput (RequestInputToStoreInPendingRequests inp) = inp

getRequestInputToStore
  :: forall (request :: Type). RequestInput request -> Maybe request
getRequestInputToStore (RequestInput _) = Nothing
getRequestInputToStore (RequestInputToStoreInPendingRequests inp) = Just inp

type RequestBody = String

type OgmiosListeners =
  { utxo :: ListenerSet TransactionInput Ogmios.UtxoQR
  , utxosAt :: ListenerSet Ogmios.OgmiosAddress Ogmios.UtxoQR
  , chainTip :: ListenerSet Unit Ogmios.ChainTipQR
  , submit :: ListenerSet (TxHash /\ CborBytes) Ogmios.SubmitTxR
  , evaluate :: ListenerSet CborBytes Ogmios.TxEvaluationR
  , getProtocolParameters :: ListenerSet Unit Ogmios.ProtocolParameters
  , eraSummaries :: ListenerSet Unit Ogmios.EraSummaries
  , currentEpoch :: ListenerSet Unit Ogmios.CurrentEpoch
  , systemStart :: ListenerSet Unit Ogmios.SystemStart
  , acquireMempool :: ListenerSet Unit Ogmios.MempoolSnapshotAcquired
  , mempoolHasTx :: ListenerSet TxHash Boolean
  }

type DatumCacheListeners =
  { getDatumByHash :: ListenerSet DataHash GetDatumByHashR
  , getDatumsByHashes :: ListenerSet (Array DataHash) GetDatumsByHashesR
  , getTxByHash :: ListenerSet TxHash GetTxByHashR
  }

-- convenience type for adding additional query types later
type ListenerSet (request :: Type) (response :: Type) =
  { addMessageListener ::
      ListenerId
      -> (Either DispatchError response -> Effect Unit)
      -> Effect Unit
  , removeMessageListener :: ListenerId -> Effect Unit
  -- ^ Removes ID from dispatch map and pending requests queue.
  , addRequest :: ListenerId -> RequestBody /\ Maybe request -> Effect Unit
  -- ^ Saves request body until the request is fulfilled. The body is used
  --  to replay requests in case of a WebSocket failure.
  }

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet
  :: forall (request :: Type) (response :: Type)
   . DispatchIdMap response
  -> PendingRequests request
  -> ListenerSet request response
mkListenerSet dim pr =
  { addMessageListener:
      \id func -> do
        Ref.modify_ (MultiMap.insert id func) dim
  , removeMessageListener:
      \id -> do
        Ref.modify_ (MultiMap.delete id) dim
        Ref.modify_ (Map.delete id) pr
  , addRequest:
      \id req ->
        Ref.modify_ (Map.insert id req) pr
  }

-- | Builds an Ogmios request action using `QueryM`
mkOgmiosRequest
  :: forall (request :: Type) (response :: Type)
   . JsonWsp.JsonWspCall request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkOgmiosRequest jsonWspCall getLs inp = do
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  websocket <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  r <- mkRequest listeners' websocket jsonWspCall getLs inp
  pure r

-- liftEffect $ log $ "response: " <>  show r

-- | Builds an Ogmios request action using `Aff`
mkOgmiosRequestAff
  :: forall (request :: Type) (response :: Type)
   . OgmiosWebSocket
  -> Logger
  -> JsonWsp.JsonWspCall request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> Aff response
mkOgmiosRequestAff ogmiosWs = mkRequestAff
  (listeners ogmiosWs)
  (underlyingWebSocket ogmiosWs)

-- | Builds a Datum Cache request action using `QueryM`
mkDatumCacheRequest
  :: forall (request :: Type) (response :: Type)
   . JsonWsp.JsonWspCall request response
  -> (DatumCacheListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkDatumCacheRequest jsonWspCall getLs inp = do
  listeners' <- asks $ listeners <<< _.datumCacheWs <<< _.runtime
  websocket <- asks $ underlyingWebSocket <<< _.datumCacheWs <<< _.runtime
  mkRequest listeners' websocket jsonWspCall getLs inp

-- | Builds a Datum Cache request action using `Aff`
mkDatumCacheRequestAff
  :: forall (request :: Type) (response :: Type)
   . DatumCacheWebSocket
  -> Logger
  -> JsonWsp.JsonWspCall request response
  -> (DatumCacheListeners -> ListenerSet request response)
  -> request
  -> Aff response
mkDatumCacheRequestAff datumCacheWs = mkRequestAff
  (listeners datumCacheWs)
  (underlyingWebSocket datumCacheWs)

mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkRequest listeners' ws jsonWspCall getLs inp = do
  logger <- getLogger
  liftAff $ mkRequestAff listeners' ws logger jsonWspCall getLs inp

type Logger = LogLevel -> String -> Effect Unit

mkLogger
  :: LogLevel
  -> Maybe (Message -> Aff Unit)
  -> Logger
mkLogger logLevel mbCustomLogger level message =
  case mbCustomLogger of
    Nothing -> logString logLevel level message
    Just logger -> liftEffect do
      timestamp <- now
      launchAff_ $ logger { level, message, tags: Map.empty, timestamp }

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
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> Aff response
mkRequestAff listeners' webSocket logger jsonWspCall getLs =
  mkRequestAff' listeners' webSocket logger jsonWspCall getLs
    <<< RequestInput

mkRequestAff'
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> Logger
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> RequestInput request
  -> Aff response
mkRequestAff' listeners' webSocket logger jsonWspCall getLs inp = do
  { body, id } <-
    liftEffect $ JsonWsp.buildRequest jsonWspCall (getRequestInput inp)
  let
    respLs :: ListenerSet request response
    respLs = getLs listeners'

    sBody :: RequestBody
    sBody = stringifyAeson body

    affFunc :: (Either Error response -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      _ <- respLs.addMessageListener id
        ( \result -> do
            respLs.removeMessageListener id
            case result of
              Left (ListenerCancelled _) -> pure unit
              _ -> cont (lmap dispatchErrorToError result)
        )
      respLs.addRequest id (sBody /\ getRequestInputToStore inp)
      _wsSend webSocket (logger Debug) sBody
      -- Uncomment this code fragment to test `SubmitTx` request resend logic:
      -- when (isJust $ getRequestInputToStore inp) $
      --   _wsReconnect webSocket
      pure $ Canceler $ \err -> do
        liftEffect $ respLs.removeMessageListener id
        liftEffect $ throwError $ err
  makeAff affFunc

-------------------------------------------------------------------------------
-- Dispatch Setup
--------------------------------------------------------------------------------

data DispatchError
  = JsError Error
  | JsonError JsonDecodeError
  -- Server response has been parsed succesfully, but it contains error
  -- message
  | FaultError Aeson
  -- The listener that was added for this message has been cancelled
  | ListenerCancelled ListenerId

instance Show DispatchError where
  show (JsError err) = "(JsError (message " <> show (message err) <> "))"
  show (JsonError jsonErr) = "(JsonError " <> show jsonErr <> ")"
  show (FaultError aeson) = "(FaultError " <> show aeson <> ")"
  show (ListenerCancelled listenerId) =
    "(ListenerCancelled " <> show listenerId <> ")"

dispatchErrorToError :: DispatchError -> Error
dispatchErrorToError (JsError err) = err
dispatchErrorToError (JsonError err) = error $ show err
dispatchErrorToError (FaultError err) =
  error $ "Server responded with `fault`: " <> stringifyAeson err
dispatchErrorToError (ListenerCancelled listenerId) =
  error $ "Listener cancelled (" <> listenerId <> ")"

-- A function which accepts some unparsed Json, and checks it against one or
-- more possible types to perform an appropriate effect (such as supplying the
-- parsed result to an async fiber/Aff listener)
type WebsocketDispatch =
  String -> Effect (Either DispatchError (Effect Unit))

-- A mutable queue of requests
type DispatchIdMap response = Ref
  (MultiMap ListenerId (Either DispatchError response -> Effect Unit))

-- an immutable queue of response type handlers
ogmiosMessageDispatch
  :: { utxoDispatchMap :: DispatchIdMap Ogmios.UtxoQR
     , utxosAtDispatchMap :: DispatchIdMap Ogmios.UtxoQR
     , chainTipDispatchMap :: DispatchIdMap Ogmios.ChainTipQR
     , evaluateTxDispatchMap :: DispatchIdMap Ogmios.TxEvaluationR
     , getProtocolParametersDispatchMap ::
         DispatchIdMap Ogmios.ProtocolParameters
     , submitDispatchMap :: DispatchIdMap Ogmios.SubmitTxR
     , eraSummariesDispatchMap :: DispatchIdMap Ogmios.EraSummaries
     , currentEpochDispatchMap :: DispatchIdMap Ogmios.CurrentEpoch
     , systemStartDispatchMap :: DispatchIdMap Ogmios.SystemStart
     , acquireMempoolDispatchMap :: DispatchIdMap Ogmios.MempoolSnapshotAcquired
     , mempoolHasTxDispatchMap :: DispatchIdMap Boolean
     }
  -> Array WebsocketDispatch
ogmiosMessageDispatch
  { utxoDispatchMap
  , utxosAtDispatchMap
  , chainTipDispatchMap
  , evaluateTxDispatchMap
  , getProtocolParametersDispatchMap
  , submitDispatchMap
  , eraSummariesDispatchMap
  , currentEpochDispatchMap
  , systemStartDispatchMap
  , acquireMempoolDispatchMap
  , mempoolHasTxDispatchMap
  } =
  [ queryDispatch utxoDispatchMap
  , queryDispatch utxosAtDispatchMap
  , queryDispatch chainTipDispatchMap
  , queryDispatch evaluateTxDispatchMap
  , queryDispatch getProtocolParametersDispatchMap
  , queryDispatch submitDispatchMap
  , queryDispatch eraSummariesDispatchMap
  , queryDispatch currentEpochDispatchMap
  , queryDispatch systemStartDispatchMap
  , queryDispatch acquireMempoolDispatchMap
  , queryDispatch mempoolHasTxDispatchMap
  ]

datumCacheMessageDispatch
  :: { getDatumByHashDispatchMap :: DispatchIdMap GetDatumByHashR
     , getDatumsByHashesDispatchMap :: DispatchIdMap GetDatumsByHashesR
     , getTxByHashDispatchMap :: DispatchIdMap GetTxByHashR
     }
  -> Array WebsocketDispatch
datumCacheMessageDispatch
  { getDatumByHashDispatchMap
  , getDatumsByHashesDispatchMap
  , getTxByHashDispatchMap
  } =
  [ queryDispatch getDatumByHashDispatchMap
  , queryDispatch getDatumsByHashesDispatchMap
  , queryDispatch getTxByHashDispatchMap
  ]

-- each query type will have a corresponding ref that lives in ReaderT config or similar
-- for utxoQueryDispatch, the `a` parameter will be `UtxoQR` or similar
-- the add and remove listener functions will know to grab the correct mutable dispatch, if one exists.
createMutableDispatch
  :: forall (response :: Type). Effect (DispatchIdMap response)
createMutableDispatch = Ref.new MultiMap.empty

createPendingRequests
  :: forall (request :: Type). Effect (PendingRequests request)
createPendingRequests = Ref.new Map.empty

-- we parse out the utxo query result, then check if we're expecting a result
-- with the provided id, if we are then we dispatch to the effect that is
-- waiting on this result
queryDispatch
  :: forall (response :: Type)
   . DecodeAeson response
  => Show response
  => DispatchIdMap response
  -> String
  -> Effect (Either DispatchError (Effect Unit))
queryDispatch ref str = do
  let eiAeson = parseJsonStringToAeson str
  -- Parse response id
  case parseJsonWspResponseId =<< eiAeson of
    Left parseError ->
      pure $ Left $ JsonError parseError
    Right reflection -> do
      -- Get callback action
      withAction reflection case _ of
        Nothing -> Left (ListenerCancelled reflection)
        Just action -> do
          -- Parse response
          Right $ action $
            case JsonWsp.parseJsonWspResponse =<< eiAeson of
              Left parseError -> Left $ JsonError parseError
              Right { result: Just result } -> Right result
              -- If result is empty, then fault must be present
              Right { result: Nothing, fault: Just fault } ->
                Left $ FaultError fault
              -- Otherwise, our implementation is broken.
              Right { result: Nothing, fault: Nothing } ->
                Left $ JsError $ error impossibleErrorMsg
  where
  impossibleErrorMsg =
    "Impossible happened: response does not contain neither "
      <> "`fault` nor `result`, please report as bug. Response: "
      <> str

  withAction
    :: ListenerId
    -> ( Maybe (Either DispatchError response -> Effect Unit)
         -> Either DispatchError (Effect Unit)
       )
    -> Effect (Either DispatchError (Effect Unit))
  withAction reflection cb = do
    idMap <- Ref.read ref
    let
      mbAction =
        MultiMap.lookup reflection idMap
          :: Maybe (Either DispatchError response -> Effect Unit)
    pure $ cb mbAction

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: JsonDecodeError
defaultErr = TypeMismatch "default error"

defaultMessageListener
  :: Logger
  -> Array WebsocketDispatch
  -> String
  -> Effect Unit
defaultMessageListener logger dispatchArray msg = do
  -- here, we need to fold the input over the array of functions until we get
  -- a success, then execute the effect.
  -- using a fold instead of a traverse allows us to skip a bunch of execution
  eAction :: Either DispatchError (Effect Unit) <- foldl
    (messageFoldF msg)
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
  :: String
  -> Effect (Either DispatchError (Effect Unit))
  -> (String -> (Effect (Either DispatchError (Effect Unit))))
  -> Effect (Either DispatchError (Effect Unit))
messageFoldF msg acc' func = do
  acc <- acc'
  if isRight acc then acc' else func msg
