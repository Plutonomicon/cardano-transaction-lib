-- | TODO docstring
module QueryM
  ( ClientError(..)
  , DatumCacheListeners
  , DatumCacheWebSocket
  , DefaultQueryConfig
  , DispatchError(..)
  , DispatchIdMap
  , FeeEstimate(..)
  , ListenerSet
  , OgmiosListeners
  , OgmiosWebSocket
  , PendingRequests
  , QueryConfig
  , QueryM
  , QueryMExtended
  , RdmrPtrExUnits(..)
  , RequestBody
  , WebSocket
  , allowError
  , applyArgs
  , calculateMinFee
  , evalTxExecutionUnits
  , getChainTip
  , getDatumByHash
  , getDatumsByHashes
  , getProtocolParametersAff
  , getWalletAddress
  , getWalletCollateral
  , liftQueryM
  , listeners
  , postAeson
  , mkDatumCacheWebSocketAff
  , mkOgmiosRequest
  , mkOgmiosRequestAff
  , mkOgmiosWebSocketAff
  , module ServerConfig
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , ownStakePubKeyHash
  , runQueryM
  , signTransaction
  , scriptToAeson
  , submitTxOgmios
  , traceQueryConfig
  , underlyingWebSocket
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
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
import Cardano.Types.Transaction (Transaction(Transaction))
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value (Coin)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Logger.Trans (LoggerT, runLoggerT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT, ask, asks)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, isRight, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(POST))
import Data.Log.Level (LogLevel(Trace, Debug, Error))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), delay, launchAff_, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message, throw)
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
  , _wsWatch
  )
import QueryM.DatumCacheWsp (GetDatumByHashR, GetDatumsByHashesR)
import QueryM.DatumCacheWsp as DcWsp
import QueryM.JsonWsp (parseJsonWspResponseId)
import QueryM.JsonWsp as JsonWsp
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
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , mkHttpUrl
  , mkOgmiosDatumCacheWsUrl
  , mkWsUrl
  )
import QueryM.UniqueId (ListenerId)
import Serialization (convertTransaction, toBytes) as Serialization
import Serialization.Address
  ( Address
  , NetworkId(TestnetId)
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , addressPaymentCred
  , stakeCredentialToKeyHash
  )
import Serialization.PlutusData (convertPlutusData) as Serialization
import Types.ByteArray (ByteArray, byteArrayToHex)
import Types.CborBytes (CborBytes)
import Types.Chain as Chain
import Types.Datum (DataHash, Datum)
import Types.MultiMap (MultiMap)
import Types.MultiMap as MultiMap
import Types.Natural (Natural)
import Types.PlutusData (PlutusData)
import Types.PubKeyHash (PaymentPubKeyHash, PubKeyHash, StakePubKeyHash)
import Types.Scripts (PlutusScript)
import Types.UsedTxOuts (newUsedTxOuts, UsedTxOuts)
import Untagged.Union (asOneOf)
import Wallet (Wallet(Gero, Nami, KeyWallet), Cip30Connection, Cip30Wallet)

-- This module defines an Aff interface for Ogmios Websocket Queries
-- Since WebSockets do not define a mechanism for linking request/response
-- Or for verifying that the connection is live, those concerns are addressed
-- here

------------------------

-- when we add multiple query backends or wallets,
-- we just need to extend this type
type QueryConfig (r :: Row Type) =
  { ogmiosWs :: OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , serverConfig :: ServerConfig
  , wallet :: Maybe Wallet
  -- should probably be more tightly coupled with a wallet
  , usedTxOuts :: UsedTxOuts
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , pparams :: Ogmios.ProtocolParameters
  | r
  }

type DefaultQueryConfig = QueryConfig ()

type QueryM (a :: Type) = ReaderT DefaultQueryConfig (LoggerT Aff) a

type QueryMExtended (r :: Row Type) (a :: Type) = ReaderT (QueryConfig r)
  (LoggerT Aff)
  a

liftQueryM :: forall (r :: Row Type) (a :: Type). QueryM a -> QueryMExtended r a
liftQueryM = withReaderT toDefaultQueryConfig
  where
  toDefaultQueryConfig :: QueryConfig r -> DefaultQueryConfig
  toDefaultQueryConfig c =
    { ogmiosWs: c.ogmiosWs
    , datumCacheWs: c.datumCacheWs
    , serverConfig: c.serverConfig
    , wallet: c.wallet
    , usedTxOuts: c.usedTxOuts
    , networkId: c.networkId
    , logLevel: c.logLevel
    , pparams: c.pparams
    }

runQueryM :: forall (a :: Type). DefaultQueryConfig -> QueryM a -> Aff a
runQueryM cfg =
  flip runLoggerT (logWithLevel cfg.logLevel) <<< flip runReaderT cfg

-- A `DefaultQueryConfig` useful for testing, with `logLevel` set to `Trace`
traceQueryConfig :: Aff DefaultQueryConfig
traceQueryConfig = do
  ogmiosWs <- mkOgmiosWebSocketAff logLevel defaultOgmiosWsConfig
  datumCacheWs <- mkDatumCacheWebSocketAff logLevel defaultDatumCacheWsConfig
  usedTxOuts <- newUsedTxOuts
  pparams <- getProtocolParametersAff ogmiosWs logLevel
  pure
    { ogmiosWs
    , datumCacheWs
    , serverConfig: defaultServerConfig
    , wallet: Nothing
    , usedTxOuts
    , networkId: TestnetId
    , logLevel
    , pparams
    }
  where
  logLevel :: LogLevel
  logLevel = Trace

getProtocolParametersAff
  :: OgmiosWebSocket -> LogLevel -> Aff Ogmios.ProtocolParameters
getProtocolParametersAff ogmiosWs logLevel =
  mkOgmiosRequestAff ogmiosWs logLevel Ogmios.queryProtocolParametersCall
    _.getProtocolParameters
    unit

--------------------------------------------------------------------------------
-- OGMIOS LOCAL STATE QUERY PROTOCOL
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
-- OGMIOS LOCAL TX SUBMISSION PROTOCOL
--------------------------------------------------------------------------------

submitTxOgmios :: CborBytes -> QueryM Ogmios.SubmitTxR
submitTxOgmios txCbor = mkOgmiosRequest Ogmios.submitTxCall _.submit txCbor

--------------------------------------------------------------------------------
-- DATUM CACHE QUERIES
--------------------------------------------------------------------------------

getDatumByHash :: DataHash -> QueryM (Maybe Datum)
getDatumByHash hash = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumByHashCall _.getDatumByHash hash

getDatumsByHashes :: Array DataHash -> QueryM (Map DataHash Datum)
getDatumsByHashes hashes = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumsByHashesCall _.getDatumsByHashes hashes

allowError
  :: forall (a :: Type). (Either Error a -> Effect Unit) -> a -> Effect Unit
allowError func = func <<< Right

--------------------------------------------------------------------------------
-- Wallet
--------------------------------------------------------------------------------

getWalletAddress :: QueryM (Maybe Address)
getWalletAddress = do
  networkId <- asks _.networkId
  withMWalletAff case _ of
    Nami nami -> callCip30Wallet nami _.getWalletAddress
    Gero gero -> callCip30Wallet gero _.getWalletAddress
    KeyWallet kw -> Just <$> kw.address networkId

getWalletCollateral :: QueryM (Maybe TransactionUnspentOutput)
getWalletCollateral = withMWalletAff case _ of
  Nami nami -> callCip30Wallet nami _.getCollateral
  Gero gero -> callCip30Wallet gero _.getCollateral
  KeyWallet _ -> liftEffect $ throw "Not implemented"

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = withMWalletAff case _ of
  Nami nami -> callCip30Wallet nami \nw -> flip nw.signTx tx
  Gero gero -> callCip30Wallet gero \nw -> flip nw.signTx tx
  KeyWallet kw -> Just <$> kw.signTx tx

ownPubKeyHash :: QueryM (Maybe PubKeyHash)
ownPubKeyHash = do
  mbAddress <- getWalletAddress
  pure $
    wrap <$> (mbAddress >>= (addressPaymentCred >=> stakeCredentialToKeyHash))

ownPaymentPubKeyHash :: QueryM (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = map wrap <$> ownPubKeyHash

ownStakePubKeyHash :: QueryM (Maybe StakePubKeyHash)
ownStakePubKeyHash = do
  mbAddress <- getWalletAddress
  pure do
    baseAddress <- mbAddress >>= baseAddressFromAddress
    wrap <<< wrap <$> stakeCredentialToKeyHash
      (baseAddressDelegationCred baseAddress)

withMWalletAff
  :: forall (a :: Type). (Wallet -> Aff (Maybe a)) -> QueryM (Maybe a)
withMWalletAff act = asks _.wallet >>= maybe (pure Nothing) (liftAff <<< act)

callCip30Wallet
  :: forall (a :: Type)
   . Cip30Wallet
  -> (Cip30Wallet -> (Cip30Connection -> Aff a))
  -> Aff a
callCip30Wallet wallet act = act wallet wallet.connection

-- The server will respond with a stringified integer value for the fee estimate
newtype FeeEstimate = FeeEstimate BigInt

derive instance Newtype FeeEstimate _

instance DecodeAeson FeeEstimate where
  decodeAeson str =
    map FeeEstimate
      <<< note (TypeMismatch "Expected a `BigInt`")
      <<< BigInt.fromString
      =<< caseAesonString
        (Left $ TypeMismatch "Expected a stringified `BigInt`")
        Right
        str

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

txToHex :: Transaction -> Effect String
txToHex tx =
  byteArrayToHex
    <<< Serialization.toBytes
    <<< asOneOf
    <$> Serialization.convertTransaction tx

-- Query the Haskell server for the minimum transaction fee
calculateMinFee :: Transaction -> QueryM (Either ClientError Coin)
calculateMinFee tx@(Transaction { body: Transaction.TxBody body }) = do
  txHex <- liftEffect (txToHex tx)
  url <- mkServerEndpointUrl "fees"
  liftAff (postAeson url (encodeAeson { count: witCount, tx: txHex }))
    <#> map (wrap <<< unwrap :: FeeEstimate -> Coin)
      <<< handleAffjaxResponse
  where
  -- Fee estimation occurs before balancing the transaction, so we need to know
  -- the expected number of witnesses to use the cardano-api fee estimation
  -- functions
  --
  -- We obtain the expected number of key witnesses for the transaction, with
  -- the following assumptions:
  --   * if `requiredSigners` is `Nothing`, add one key witness for the
  --     current wallet. Thus there should normally be at least one witness
  --     for any transaction
  --   * otherwise, the expected number of signers has been implicitly
  --     specified by the `requiredSigners` field; take the length of the
  --     array
  --   * this assumes of course that users will not pass `Just mempty` for the
  --     required signers
  witCount :: UInt
  witCount = maybe one UInt.fromInt $ length <$> body.requiredSigners

newtype RdmrPtrExUnits = RdmrPtrExUnits
  { rdmrPtrTag :: Int
  , rdmrPtrIdx :: Natural
  , exUnitsMem :: Natural
  , exUnitsSteps :: Natural
  }

derive instance Generic RdmrPtrExUnits _

instance Show RdmrPtrExUnits where
  show = genericShow

derive newtype instance DecodeAeson RdmrPtrExUnits

evalTxExecutionUnits
  :: Transaction -> QueryM (Either ClientError (Array RdmrPtrExUnits))
evalTxExecutionUnits tx = do
  txHex <- liftEffect (txToHex tx)
  url <- mkServerEndpointUrl "eval-ex-units"
  liftAff $ postAeson url (encodeAeson { tx: txHex })
    <#> handleAffjaxResponse

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgs
  :: forall (a :: Type)
   . Newtype a PlutusScript
  => DecodeAeson a
  => a
  -> Array PlutusData
  -> QueryM (Either ClientError a)
applyArgs script args = case traverse plutusDataToAeson args of
  Nothing -> pure $ Left $ ClientEncodingError "Failed to convert script args"
  Just ps -> do
    let
      reqBody :: Aeson
      reqBody = encodeAeson
        $ Object.fromFoldable
            [ "script" /\ scriptToAeson (unwrap script)
            , "args" /\ encodeAeson ps
            ]
    url <- mkServerEndpointUrl "apply-args"
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

mkServerEndpointUrl :: String -> QueryM Url
mkServerEndpointUrl path = asks $ (_ <> "/" <> path)
  <<< mkHttpUrl
  <<< _.serverConfig

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
  :: LogLevel
  -> ServerConfig
  -> (Either Error OgmiosWebSocket -> Effect Unit)
  -> Effect Canceler
mkOgmiosWebSocket' lvl serverCfg continue = do
  utxoDispatchMap <- createMutableDispatch
  chainTipDispatchMap <- createMutableDispatch
  evaluateTxDispatchMap <- createMutableDispatch
  getProtocolParametersDispatchMap <- createMutableDispatch
  submitDispatchMap <- createMutableDispatch
  eraSummariesDispatchMap <- createMutableDispatch
  currentEpochDispatchMap <- createMutableDispatch
  systemStartDispatchMap <- createMutableDispatch
  utxoPendingRequests <- createPendingRequests
  chainTipPendingRequests <- createPendingRequests
  evaluateTxPendingRequests <- createPendingRequests
  getProtocolParametersPendingRequests <- createPendingRequests
  submitPendingRequests <- createPendingRequests
  eraSummariesPendingRequests <- createPendingRequests
  currentEpochPendingRequests <- createPendingRequests
  systemStartPendingRequests <- createPendingRequests
  let
    messageDispatch = ogmiosMessageDispatch
      { utxoDispatchMap
      , chainTipDispatchMap
      , evaluateTxDispatchMap
      , getProtocolParametersDispatchMap
      , submitDispatchMap
      , eraSummariesDispatchMap
      , currentEpochDispatchMap
      , systemStartDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let
    sendRequest = _wsSend ws (logString lvl Debug)
    resendPendingRequests = do
      Ref.read utxoPendingRequests >>= traverse_ sendRequest
      Ref.read chainTipPendingRequests >>= traverse_ sendRequest
      Ref.read evaluateTxPendingRequests >>= traverse_ sendRequest
      Ref.read getProtocolParametersPendingRequests >>= traverse_ sendRequest
      Ref.read submitPendingRequests >>= traverse_ sendRequest
      Ref.read eraSummariesPendingRequests >>= traverse_ sendRequest
      Ref.read currentEpochPendingRequests >>= traverse_ sendRequest
      Ref.read systemStartPendingRequests >>= traverse_ sendRequest
      logString lvl Debug "Resent all pending requests"
    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError errMessage = do
      _wsClose ws
      logger Error $
        "First connection to Ogmios WebSocket failed. Terminating. Error: " <>
          errMessage
      _wsClose ws
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws (logger Error) onFirstConnectionError
  hasConnectedOnceRef <- Ref.new false
  _onWsConnect ws $ Ref.read hasConnectedOnceRef >>= case _ of
    true -> do
      logger Debug
        "Ogmios WS connection re-established, resending pending requests..."
      resendPendingRequests
    false -> do
      logger Debug "Ogmios Connection established"
      Ref.write true hasConnectedOnceRef
      _removeOnWsError ws firstConnectionErrorRef
      _wsWatch ws (logger Debug) do
        logger Debug "Ogmios WebSocket terminated by timeout. Reconnecting..."
        _wsReconnect ws
      _onWsMessage ws (logger Debug) $ defaultMessageListener lvl
        messageDispatch
      void $ _onWsError ws (logger Error) $ \err -> do
        logString lvl Debug $
          "Ogmios WebSocket error (" <> err <> "). Reconnecting..."
        launchAff_ do
          delay (wrap 500.0)
          liftEffect $ _wsReconnect ws
      continue $ Right $ WebSocket ws
        { utxo: mkListenerSet utxoDispatchMap utxoPendingRequests
        , chainTip: mkListenerSet chainTipDispatchMap chainTipPendingRequests
        , evaluate: mkListenerSet evaluateTxDispatchMap
            evaluateTxPendingRequests
        , getProtocolParameters: mkListenerSet
            getProtocolParametersDispatchMap
            getProtocolParametersPendingRequests
        , submit: mkListenerSet submitDispatchMap submitPendingRequests
        , eraSummaries:
            mkListenerSet eraSummariesDispatchMap eraSummariesPendingRequests
        , currentEpoch:
            mkListenerSet currentEpochDispatchMap currentEpochPendingRequests
        , systemStart:
            mkListenerSet systemStartDispatchMap systemStartPendingRequests
        }
  pure $ Canceler $ \err -> liftEffect do
    _wsClose ws
    continue $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkDatumCacheWebSocket'
  :: LogLevel
  -> ServerConfig
  -> (Either Error DatumCacheWebSocket -> Effect Unit)
  -> Effect Canceler
mkDatumCacheWebSocket' lvl serverCfg continue = do
  getDatumByHashDispatchMap <- createMutableDispatch
  getDatumsByHashesDispatchMap <- createMutableDispatch
  getDatumByHashPendingRequests <- createPendingRequests
  getDatumsByHashesPendingRequests <- createPendingRequests
  let
    messageDispatch = datumCacheMessageDispatch
      { getDatumByHashDispatchMap
      , getDatumsByHashesDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkOgmiosDatumCacheWsUrl serverCfg
  let
    sendRequest = _wsSend ws (logger Debug)
    resendPendingRequests = do
      Ref.read getDatumByHashPendingRequests >>= traverse_ sendRequest
      Ref.read getDatumsByHashesPendingRequests >>= traverse_ sendRequest
    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError errMessage = do
      _wsClose ws
      logger Error $
        "First connection to Ogmios Datum Cache WebSocket failed. "
          <> "Terminating. Error: "
          <> errMessage
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws (logger Error) onFirstConnectionError
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
      _wsWatch ws (logger Debug) do
        logger Debug $ "Ogmios Datum Cache WebSocket terminated by " <>
          "timeout. Reconnecting..."
        _wsReconnect ws
      _onWsMessage ws (logger Debug) $ defaultMessageListener lvl
        messageDispatch
      void $ _onWsError ws (logger Error) $ \err -> do
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
        }
  pure $ Canceler $ \err -> liftEffect do
    _wsClose ws
    continue $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkDatumCacheWebSocketAff :: LogLevel -> ServerConfig -> Aff DatumCacheWebSocket
mkDatumCacheWebSocketAff lvl = makeAff <<< mkDatumCacheWebSocket' lvl

mkOgmiosWebSocketAff :: LogLevel -> ServerConfig -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff lvl = makeAff <<< mkOgmiosWebSocket' lvl

-- getter
underlyingWebSocket :: forall (a :: Type). WebSocket a -> JsWebSocket
underlyingWebSocket (WebSocket ws _) = ws

-- getter
listeners :: forall (listeners :: Type). WebSocket listeners -> listeners
listeners (WebSocket _ ls) = ls

type PendingRequests (request :: Type) = Ref (Map ListenerId RequestBody)

type RequestBody = String

type OgmiosListeners =
  { utxo :: ListenerSet Ogmios.OgmiosAddress Ogmios.UtxoQR
  , chainTip :: ListenerSet Unit Ogmios.ChainTipQR
  , submit :: ListenerSet { txCbor :: ByteArray } Ogmios.SubmitTxR
  , evaluate :: ListenerSet { txCbor :: ByteArray } Ogmios.TxEvaluationR
  , getProtocolParameters :: ListenerSet Unit Ogmios.ProtocolParameters
  , eraSummaries :: ListenerSet Unit Ogmios.EraSummaries
  , currentEpoch :: ListenerSet Unit Ogmios.CurrentEpoch
  , systemStart :: ListenerSet Unit Ogmios.SystemStart
  }

type DatumCacheListeners =
  { getDatumByHash :: ListenerSet DataHash GetDatumByHashR
  , getDatumsByHashes :: ListenerSet (Array DataHash) GetDatumsByHashesR
  }

-- convenience type for adding additional query types later
type ListenerSet (request :: Type) (response :: Type) =
  { addMessageListener ::
      ListenerId
      -> (Either DispatchError response -> Effect Unit)
      -> Effect Unit
  , removeMessageListener :: ListenerId -> Effect Unit
  -- ^ Removes ID from dispatch map and pending requests queue.
  , addRequest :: ListenerId -> RequestBody -> Effect Unit
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
mkOgmiosRequest = mkRequest
  (listeners <<< _.ogmiosWs <$> ask)
  (underlyingWebSocket <<< _.ogmiosWs <$> ask)

-- | Builds an Ogmios request action using `Aff`
mkOgmiosRequestAff
  :: forall (request :: Type) (response :: Type)
   . OgmiosWebSocket
  -> LogLevel
  -> JsonWsp.JsonWspCall request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> Aff response
mkOgmiosRequestAff ogmiosWs = mkRequestAff
  (listeners ogmiosWs)
  (underlyingWebSocket ogmiosWs)

mkDatumCacheRequest
  :: forall (request :: Type) (response :: Type)
   . JsonWsp.JsonWspCall request response
  -> (DatumCacheListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkDatumCacheRequest = mkRequest
  (listeners <<< _.datumCacheWs <$> ask)
  (underlyingWebSocket <<< _.datumCacheWs <$> ask)

-- | Builds an Ogmios request action using `QueryM`
mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . QueryM listeners
  -> QueryM JsWebSocket
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkRequest getListeners getWebSocket jsonWspCall getLs inp = do
  ws <- getWebSocket
  listeners' <- getListeners
  logLevel <- asks _.logLevel
  liftAff $ mkRequestAff listeners' ws logLevel jsonWspCall getLs inp

-- | Builds an Ogmios request action using `Aff`
mkRequestAff
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> LogLevel
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> Aff response
mkRequestAff listeners' webSocket logLevel jsonWspCall getLs inp = do
  { body, id } <- liftEffect $ JsonWsp.buildRequest jsonWspCall inp
  let
    respLs :: ListenerSet request response
    respLs = getLs listeners'

    affFunc :: (Either Error response -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        sBody :: RequestBody
        sBody = stringifyAeson $ encodeAeson body
      _ <- respLs.addMessageListener id
        ( \result -> do
            respLs.removeMessageListener id
            cont (lmap dispatchErrorToError result)
        )
      respLs.addRequest id sBody
      _wsSend webSocket (logString logLevel Debug) sBody
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
  | ListenerCancelled

instance Show DispatchError where
  show (JsError err) = "(JsError (message " <> show (message err) <> "))"
  show (JsonError jsonErr) = "(JsonError " <> show jsonErr <> ")"
  show (FaultError aeson) = "(FaultError " <> show aeson <> ")"
  show ListenerCancelled = "ListenerCancelled"

dispatchErrorToError :: DispatchError -> Error
dispatchErrorToError (JsError err) = err
dispatchErrorToError (JsonError err) = error $ show err
dispatchErrorToError (FaultError err) =
  error $ "Server responded with `fault`: " <> stringifyAeson err
dispatchErrorToError ListenerCancelled =
  error $ "Listener cancelled"

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
     , chainTipDispatchMap :: DispatchIdMap Ogmios.ChainTipQR
     , evaluateTxDispatchMap :: DispatchIdMap Ogmios.TxEvaluationR
     , getProtocolParametersDispatchMap ::
         DispatchIdMap Ogmios.ProtocolParameters
     , submitDispatchMap :: DispatchIdMap Ogmios.SubmitTxR
     , eraSummariesDispatchMap :: DispatchIdMap Ogmios.EraSummaries
     , currentEpochDispatchMap :: DispatchIdMap Ogmios.CurrentEpoch
     , systemStartDispatchMap :: DispatchIdMap Ogmios.SystemStart
     }
  -> Array WebsocketDispatch
ogmiosMessageDispatch
  { utxoDispatchMap
  , chainTipDispatchMap
  , evaluateTxDispatchMap
  , getProtocolParametersDispatchMap
  , submitDispatchMap
  , eraSummariesDispatchMap
  , currentEpochDispatchMap
  , systemStartDispatchMap
  } =
  [ queryDispatch utxoDispatchMap
  , queryDispatch chainTipDispatchMap
  , queryDispatch evaluateTxDispatchMap
  , queryDispatch getProtocolParametersDispatchMap
  , queryDispatch submitDispatchMap
  , queryDispatch eraSummariesDispatchMap
  , queryDispatch currentEpochDispatchMap
  , queryDispatch systemStartDispatchMap
  ]

datumCacheMessageDispatch
  :: { getDatumByHashDispatchMap :: DispatchIdMap GetDatumByHashR
     , getDatumsByHashesDispatchMap :: DispatchIdMap GetDatumsByHashesR
     }
  -> Array WebsocketDispatch
datumCacheMessageDispatch
  { getDatumByHashDispatchMap
  , getDatumsByHashesDispatchMap
  } =
  [ queryDispatch getDatumByHashDispatchMap
  , queryDispatch getDatumsByHashesDispatchMap
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
        Nothing -> Left $ JsError $ error $
          "Request Id " <> reflection <> " has been cancelled"
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
  :: LogLevel -> Array WebsocketDispatch -> String -> Effect Unit
defaultMessageListener lvl dispatchArray msg = do
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
            logString lvl Error $
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
