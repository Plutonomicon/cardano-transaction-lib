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
  , FeeEstimate(FeeEstimate)
  , ListenerSet
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
  , calculateMinFee
  , evaluateTxOgmios
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
  , mkDatumCacheRequest
  , queryDispatch
  , defaultMessageListener
  , mkListenerSet
  , mkOgmiosRequest
  , mkOgmiosRequestAff
  , mkOgmiosWebSocketAff
  , mkQueryRuntime
  , mkRequest
  , mkRequestAff
  , mkWalletBySpec
  , module ServerConfig
  , ownPaymentPubKeyHash
  , ownPubKeyHash
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
import Data.Array (length)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, isRight, note)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(POST))
import Data.Log.Level (LogLevel(Error, Debug))
import Data.Log.Message (Message)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for, for_, traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , delay
  , finally
  , launchAff_
  , makeAff
  , supervise
  )
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
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
  , defaultOgmiosWsConfig
  , mkHttpUrl
  , mkOgmiosDatumCacheWsUrl
  , mkWsUrl
  )
import QueryM.UniqueId (ListenerId)
import Serialization (convertTransaction, toBytes) as Serialization
import Serialization.Address
  ( Address
  , NetworkId
  , addressPaymentCred
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , stakeCredentialToKeyHash
  )
import Serialization.PlutusData (convertPlutusData) as Serialization
import Types.ByteArray (ByteArray, byteArrayToHex)
import Types.CborBytes (CborBytes)
import Types.Chain as Chain
import Types.Datum (DataHash, Datum)
import Types.MultiMap (MultiMap)
import Types.MultiMap as MultiMap
import Types.PlutusData (PlutusData)
import Types.PubKeyHash (PaymentPubKeyHash, PubKeyHash, StakePubKeyHash)
import Types.Scripts (PlutusScript)
import Types.UsedTxOuts (newUsedTxOuts, UsedTxOuts)
import Untagged.Union (asOneOf)
import Wallet
  ( Cip30Connection
  , Cip30Wallet
  , Wallet(Gero, Nami, KeyWallet)
  , mkGeroWalletAff
  , mkKeyWallet
  , mkNamiWalletAff
  )
import Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Wallet.Spec
  ( WalletSpec(UseKeys, ConnectToGero, ConnectToNami)
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
  { ctlServerConfig :: ServerConfig
  , ogmiosConfig :: ServerConfig
  , datumCacheConfig :: ServerConfig
  , networkId :: NetworkId
  , logLevel :: LogLevel
  , walletSpec :: Maybe WalletSpec
  , customLogger :: Maybe (Message -> Aff Unit)
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
  (OgmiosWebSocket /\ Ogmios.ProtocolParameters)
  DatumCacheWebSocket
  (Maybe Wallet)

mkQueryRuntime
  :: QueryConfig
  -> Aff QueryRuntime
mkQueryRuntime config = do
  usedTxOuts <- newUsedTxOuts
  QueryRuntimeModel (ogmiosWs /\ pparams) datumCacheWs wallet <- sequential $
    QueryRuntimeModel
      <$> parallel do
        ogmiosWs <- mkOgmiosWebSocketAff config.logLevel defaultOgmiosWsConfig
        pparams <- getProtocolParametersAff ogmiosWs config.logLevel
        pure $ ogmiosWs /\ pparams
      <*> parallel
        (mkDatumCacheWebSocketAff config.logLevel config.datumCacheConfig)
      <*> parallel (for config.walletSpec mkWalletBySpec)
  pure
    { ogmiosWs
    , datumCacheWs
    , wallet
    , usedTxOuts
    , pparams
    }

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
submitTxOgmios = mkOgmiosRequest Ogmios.submitTxCall _.submit

evaluateTxOgmios :: CborBytes -> QueryM Ogmios.TxEvaluationR
evaluateTxOgmios = mkOgmiosRequest Ogmios.evaluateTxCall _.evaluate

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
  networkId <- asks $ _.config >>> _.networkId
  withMWalletAff case _ of
    Nami nami -> callCip30Wallet nami _.getWalletAddress
    Gero gero -> callCip30Wallet gero _.getWalletAddress
    KeyWallet kw -> Just <$> (unwrap kw).address networkId

getWalletCollateral :: QueryM (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  mbCollateralUTxOs <- withMWalletAff case _ of
    Nami nami -> callCip30Wallet nami _.getCollateral
    Gero gero -> callCip30Wallet gero _.getCollateral
    KeyWallet _ -> liftEffect $ throw "Not implemented"
  for_ mbCollateralUTxOs \collateralUTxOs -> do
    pparams <- asks $ _.runtime >>> _.pparams
    let
      tooManyCollateralUTxOs =
        fromMaybe false do
          maxCollateralInputs <- (unwrap pparams).maxCollateralInputs
          pure $ UInt.fromInt (Array.length collateralUTxOs) >
            maxCollateralInputs
    when tooManyCollateralUTxOs do
      liftEffect $ throw tooManyCollateralUTxOsError
  pure mbCollateralUTxOs
  where
  tooManyCollateralUTxOsError =
    "Wallet returned too many UTxOs as collateral. This is likely a bug in \
    \the wallet."

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = withMWalletAff case _ of
  Nami nami -> callCip30Wallet nami \nw -> flip nw.signTx tx
  Gero gero -> callCip30Wallet gero \nw -> flip nw.signTx tx
  KeyWallet kw -> Just <$> (unwrap kw).signTx tx

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
withMWalletAff act = asks (_.runtime >>> _.wallet) >>= maybe (pure Nothing)
  (liftAff <<< act)

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
  <<< _.ctlServerConfig
  <<< _.config

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
    sendRequest = _wsSend ws (logger Debug)
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
        , getTxByHash: mkListenerSet getTxByHashDispatchMap
            getTxByHashPendingRequests
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
  (asks $ listeners <<< _.ogmiosWs <<< _.runtime)
  (asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime)

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
  (asks $ listeners <<< _.datumCacheWs <<< _.runtime)
  (asks $ underlyingWebSocket <<< _.datumCacheWs <<< _.runtime)

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
  logLevel <- asks $ _.config >>> _.logLevel
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

    sBody :: RequestBody
    sBody = stringifyAeson body

    affFunc :: (Either Error response -> Effect Unit) -> Effect Canceler
    affFunc cont = do
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
