-- | TODO docstring
module QueryM
  ( ClientError(..)
  , DatumCacheListeners
  , DatumCacheWebSocket
  , DispatchIdMap
  , DispatchError(JsError, JsonError)
  , FeeEstimate(..)
  , FinalizedTransaction(..)
  , HashedData(..)
  , module ServerConfig
  , ListenerSet
  , PendingRequests
  , RdmrPtrExUnits(..)
  , RequestBody
  , OgmiosListeners
  , OgmiosWebSocket
  , QueryConfig
  , DefaultQueryConfig
  , QueryM
  , QueryMExtended
  , WebSocket
  , liftQueryM
  , allowError
  , applyArgs
  , calculateMinFee
  , datumHash
  , traceQueryConfig
  , evalTxExecutionUnits
  , finalizeTx
  , getWalletAddress
  , getChainTip
  , getWalletCollateral
  , hashData
  , hashScript
  , listeners
  , mkDatumCacheWebSocketAff
  , mkOgmiosRequest
  , mkOgmiosWebSocketAff
  , ownPaymentPubKeyHash
  , ownPubKeyHash
  , ownStakePubKeyHash
  , runQueryM
  , signTransaction
  , signTransactionBytes
  , submitTxWallet
  , submitTxOgmios
  , underlyingWebSocket
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocks
  , cancelFetchBlocks
  ) where

import Prelude

import Aeson
import Affjax as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Cardano.Types.Value (Coin)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Logger.Trans (LoggerT, runLoggerT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, withReaderT, ask, asks)
import Data.Array (length)
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right), either, isRight, note, hush)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel(Trace, Debug, Error))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe, maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse, traverse_, for)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throw)
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
  , _wsSend
  , _wsWatch
  )
import QueryM.DatumCacheWsp
  ( CancelFetchBlocksR
  , GetDatumByHashR
  , GetDatumsByHashesR
  , StartFetchBlocksR
  )
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
  , Slot
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , baseAddressPaymentCred
  , stakeCredentialToKeyHash
  )
import Serialization.Hash (ScriptHash)
import Serialization.PlutusData (convertPlutusData) as Serialization
import Serialization.WitnessSet (convertRedeemers) as Serialization
import Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Types.Chain as Chain
import Types.Datum (Datum, DatumHash)
import Types.Interval (SlotConfig, defaultSlotConfig)
import Types.MultiMap (MultiMap)
import Types.MultiMap as MultiMap
import Types.Natural (Natural)
import Types.PlutusData (PlutusData)
import Types.PubKeyHash (PubKeyHash)
import Types.Scripts (PlutusScript)
import Types.Transaction (Transaction(Transaction))
import Types.Transaction as Transaction
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.UnbalancedTransaction (StakePubKeyHash, PaymentPubKeyHash)
import Types.UsedTxOuts (newUsedTxOuts, UsedTxOuts)
import Untagged.Union (asOneOf)
import Wallet (Wallet(Nami), NamiWallet, NamiConnection)

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
  , slotConfig :: SlotConfig
  , logLevel :: LogLevel
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
    , slotConfig: c.slotConfig
    , logLevel: c.logLevel
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
  pure
    { ogmiosWs
    , datumCacheWs
    , serverConfig: defaultServerConfig
    , wallet: Nothing
    , usedTxOuts
    , networkId: TestnetId
    , slotConfig: defaultSlotConfig
    , logLevel
    }
  where
  logLevel :: LogLevel
  logLevel = Trace

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

submitTxOgmios :: ByteArray -> QueryM Ogmios.SubmitTxR
submitTxOgmios txCbor = mkOgmiosRequest Ogmios.submitTxCall _.submit { txCbor }

--------------------------------------------------------------------------------
-- DATUM CACHE QUERIES
--------------------------------------------------------------------------------

getDatumByHash :: DatumHash -> QueryM (Maybe Datum)
getDatumByHash hash = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumByHashCall _.getDatumByHash hash

getDatumsByHashes :: Array DatumHash -> QueryM (Map DatumHash Datum)
getDatumsByHashes hashes = unwrap <$> do
  mkDatumCacheRequest DcWsp.getDatumsByHashesCall _.getDatumsByHashes hashes

startFetchBlocks :: { slot :: Slot, id :: Chain.BlockHeaderHash } -> QueryM Unit
startFetchBlocks start = void $ mkDatumCacheRequest DcWsp.startFetchBlocksCall
  _.startFetchBlocks
  start

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocks :: QueryM Unit
cancelFetchBlocks = void $ mkDatumCacheRequest DcWsp.cancelFetchBlocksCall
  _.cancelFetchBlocks
  unit

allowError
  :: forall (a :: Type). (Either Error a -> Effect Unit) -> a -> Effect Unit
allowError func = func <<< Right

--------------------------------------------------------------------------------
-- Wallet
--------------------------------------------------------------------------------

getWalletAddress :: QueryM (Maybe Address)
getWalletAddress = withMWalletAff $ case _ of
  Nami nami -> callNami nami _.getWalletAddress

getWalletCollateral :: QueryM (Maybe TransactionUnspentOutput)
getWalletCollateral = withMWalletAff $ case _ of
  Nami nami -> callNami nami _.getCollateral

signTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.Transaction)
signTransaction tx = withMWalletAff $ case _ of
  Nami nami -> callNami nami $ \nw -> flip nw.signTx tx

signTransactionBytes
  :: ByteArray -> QueryM (Maybe ByteArray)
signTransactionBytes tx = withMWalletAff $ case _ of
  Nami nami -> callNami nami $ \nw -> flip nw.signTxBytes tx

submitTxWallet
  :: Transaction.Transaction -> QueryM (Maybe Transaction.TransactionHash)
submitTxWallet tx = withMWalletAff $ case _ of
  Nami nami -> callNami nami $ \nw -> flip nw.submitTx tx

ownPubKeyHash :: QueryM (Maybe PubKeyHash)
ownPubKeyHash = do
  mbAddress <- getWalletAddress
  pure do
    baseAddress <- mbAddress >>= baseAddressFromAddress
    wrap <$> stakeCredentialToKeyHash (baseAddressPaymentCred baseAddress)

ownPaymentPubKeyHash :: QueryM (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = map wrap <$> ownPubKeyHash

ownStakePubKeyHash :: QueryM (Maybe StakePubKeyHash)
ownStakePubKeyHash = do
  mbAddress <- getWalletAddress
  pure do
    baseAddress <- mbAddress >>= baseAddressFromAddress
    wrap <$> stakeCredentialToKeyHash (baseAddressDelegationCred baseAddress)

withMWalletAff
  :: forall (a :: Type). (Wallet -> Aff (Maybe a)) -> QueryM (Maybe a)
withMWalletAff act = asks _.wallet >>= maybe (pure Nothing) (liftAff <<< act)

callNami
  :: forall (a :: Type)
   . NamiWallet
  -> (NamiWallet -> (NamiConnection -> Aff a))
  -> Aff a
callNami nami act = act nami =<< readNamiConnection nami
  where
  readNamiConnection :: NamiWallet -> Aff NamiConnection
  readNamiConnection = liftEffect <<< Ref.read <<< _.connection

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
  | ClientDecodeJsonError JsonDecodeError
  | ClientEncodingError String
  | ClientOtherError String

-- No Show instance of Affjax.Error
instance Show ClientError where
  show (ClientHttpError err) =
    "(ClientHttpError "
      <> Affjax.printError err
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
  url <- mkServerEndpointUrl
    $ "fees?tx="
        <> txHex
        <> "&count="
        <> UInt.toString witCount
  liftAff (Affjax.get Affjax.ResponseFormat.string url)
    <#> either
      (Left <<< ClientHttpError)
      ( bimap ClientDecodeJsonError (wrap <<< unwrap :: FeeEstimate -> Coin)
          <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )
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
  url <- mkServerEndpointUrl ("eval-ex-units?tx=" <> txHex)
  liftAff (Affjax.get Affjax.ResponseFormat.string url)
    <#> either
      (Left <<< ClientHttpError)
      ( lmap ClientDecodeJsonError <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )

-- | CborHex-encoded tx
newtype FinalizedTransaction = FinalizedTransaction ByteArray

derive instance Generic FinalizedTransaction _

instance Show FinalizedTransaction where
  show = genericShow

instance DecodeAeson FinalizedTransaction where
  decodeAeson =
    map FinalizedTransaction <<<
      caseAesonString (Left err) (note err <<< hexToByteArray)
    where
    err = TypeMismatch "Expected CborHex of Tx"

finalizeTx
  :: Transaction.Transaction
  -> Array Datum
  -> Array Transaction.Redeemer
  -> QueryM (Maybe FinalizedTransaction)
finalizeTx tx datums redeemers = do
  -- tx
  txHex <- liftEffect (txToHex tx)
  -- datums
  encodedDatums <- liftEffect do
    for datums \datum -> do
      byteArrayToHex <<< Serialization.toBytes <<< asOneOf
        <$> maybe'
          (\_ -> throw $ "Failed to convert plutus data: " <> show datum)
          pure
          (Serialization.convertPlutusData $ unwrap datum)
  -- redeemers
  encodedRedeemers <- liftEffect $
    byteArrayToHex <<< Serialization.toBytes <<< asOneOf <$>
      Serialization.convertRedeemers redeemers
  -- construct payload
  let
    body
      :: { tx :: String
         , datums :: Array String
         , redeemers :: String
         }
    body =
      { tx: txHex
      , datums: encodedDatums
      , redeemers: encodedRedeemers
      }
  url <- mkServerEndpointUrl "finalize"
  -- get response json
  jsonBody <-
    liftAff
      ( Affjax.post Affjax.ResponseFormat.string url
          (Just $ Affjax.RequestBody.String $ stringifyAeson $ encodeAeson body)
      ) <#> map \x -> x.body
  -- decode
  pure $ hush <<< (decodeAeson <=< parseJsonStringToAeson) =<< hush jsonBody

newtype HashedData = HashedData ByteArray

derive instance Newtype HashedData _
derive instance Generic HashedData _

instance Show HashedData where
  show = genericShow

instance DecodeAeson HashedData where
  decodeAeson =
    map HashedData <<<
      caseAesonString (Left err) (note err <<< hexToByteArray)
    where
    err :: JsonDecodeError
    err = TypeMismatch "Expected hex bytes (raw) of hashed data"

hashData :: Datum -> QueryM (Maybe HashedData)
hashData datum = do
  body <-
    liftEffect $ byteArrayToHex <<< Serialization.toBytes <<< asOneOf
      <$> maybe' (\_ -> throw $ "Failed to convert plutus data: " <> show datum)
        pure
        (Serialization.convertPlutusData $ unwrap datum)
  url <- mkServerEndpointUrl "hash-data"
  -- get response json
  jsonBody <-
    liftAff
      ( Affjax.post Affjax.ResponseFormat.string url
          (Just $ Affjax.RequestBody.String $ stringifyAeson $ encodeAeson body)
      ) <#> map \x -> x.body
  -- decode
  pure $ hush <<< (decodeAeson <=< parseJsonStringToAeson) =<< hush jsonBody

-- | Hashes an Plutus-style Datum
datumHash :: Datum -> QueryM (Maybe DatumHash)
datumHash = map (map (Transaction.DataHash <<< unwrap)) <<< hashData

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
      argsJson :: Aeson
      argsJson = encodeAeson ps

      reqBody :: Maybe Affjax.RequestBody.RequestBody
      reqBody = Just
        $ Affjax.RequestBody.String
        $ stringifyAeson
        $ encodeAeson
        $ Object.fromFoldable
            [ "script" /\ scriptToAeson (unwrap script)
            , "args" /\ argsJson
            ]
    url <- mkServerEndpointUrl "apply-args"
    liftAff (Affjax.post Affjax.ResponseFormat.string url reqBody)
      <#> either
        (Left <<< ClientHttpError)
        ( lmap ClientDecodeJsonError
            <<< (decodeAeson <=< parseJsonStringToAeson)
            <<< _.body
        )
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

hashScript
  :: forall (a :: Type) (b :: Type)
   . Newtype a PlutusScript
  => Newtype b ScriptHash
  => a
  -> QueryM (Either ClientError b)
hashScript script = do
  url <- mkServerEndpointUrl "hash-script"
  let
    reqBody :: Maybe Affjax.RequestBody.RequestBody
    reqBody = Just
      $ Affjax.RequestBody.String
      $ stringifyAeson
      $ scriptToAeson
      $ unwrap script
  liftAff (Affjax.post Affjax.ResponseFormat.string url reqBody)
    <#> either
      (Left <<< ClientHttpError)
      ( bimap ClientDecodeJsonError wrap
          <<< (decodeAeson <=< parseJsonStringToAeson)
          <<< _.body
      )

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
mkOgmiosWebSocket' lvl serverCfg cb = do
  utxoDispatchMap <- createMutableDispatch
  chainTipDispatchMap <- createMutableDispatch
  evaluateTxDispatchMap <- createMutableDispatch
  submitDispatchMap <- createMutableDispatch
  utxoPendingRequests <- createPendingRequests
  chainTipPendingRequests <- createPendingRequests
  evaluateTxPendingRequests <- createPendingRequests
  submitPendingRequests <- createPendingRequests
  let
    md = ogmiosMessageDispatch
      { utxoDispatchMap
      , chainTipDispatchMap
      , evaluateTxDispatchMap
      , submitDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let
    sendRequest = _wsSend ws (logString lvl Debug)
    onError = do
      logString lvl Debug "WS error occured, resending requests"
      Ref.read utxoPendingRequests >>= traverse_ sendRequest
      Ref.read chainTipPendingRequests >>= traverse_ sendRequest
      Ref.read evaluateTxPendingRequests >>= traverse_ sendRequest
      Ref.read submitPendingRequests >>= traverse_ sendRequest
  _onWsConnect ws do
    _wsWatch ws (logger Debug) onError
    _onWsMessage ws (logger Debug) $ defaultMessageListener lvl md
    _onWsError ws (logger Error) $ const onError
    cb $ Right $ WebSocket ws
      { utxo: mkListenerSet utxoDispatchMap utxoPendingRequests
      , chainTip: mkListenerSet chainTipDispatchMap chainTipPendingRequests
      , evaluate: mkListenerSet evaluateTxDispatchMap evaluateTxPendingRequests
      , submit: mkListenerSet submitDispatchMap submitPendingRequests
      }
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkDatumCacheWebSocket'
  :: LogLevel
  -> ServerConfig
  -> (Either Error DatumCacheWebSocket -> Effect Unit)
  -> Effect Canceler
mkDatumCacheWebSocket' lvl serverCfg cb = do
  getDatumByHashDispatchMap <- createMutableDispatch
  getDatumsByHashesDispatchMap <- createMutableDispatch
  startFetchBlocksDispatchMap <- createMutableDispatch
  cancelFetchBlocksDispatchMap <- createMutableDispatch
  getDatumByHashPendingRequests <- createPendingRequests
  getDatumsByHashesPendingRequests <- createPendingRequests
  startFetchBlocksPendingRequests <- createPendingRequests
  cancelFetchBlocksPendingRequests <- createPendingRequests
  let
    md = datumCacheMessageDispatch
      { getDatumByHashDispatchMap
      , getDatumsByHashesDispatchMap
      , startFetchBlocksDispatchMap
      , cancelFetchBlocksDispatchMap
      }
  ws <- _mkWebSocket (logger Debug) $ mkOgmiosDatumCacheWsUrl serverCfg
  let
    sendRequest = _wsSend ws (logString lvl Debug)
    onError = do
      logString lvl Debug "Datum Cache: WS error occured, resending requests"
      Ref.read getDatumByHashPendingRequests >>= traverse_ sendRequest
      Ref.read getDatumsByHashesPendingRequests >>= traverse_ sendRequest
      Ref.read startFetchBlocksPendingRequests >>= traverse_ sendRequest
      Ref.read cancelFetchBlocksPendingRequests >>= traverse_ sendRequest
  _onWsConnect ws $ do
    _wsWatch ws (logger Debug) onError
    _onWsMessage ws (logger Debug) $ defaultMessageListener lvl md
    _onWsError ws (logger Error) $ const onError
    cb $ Right $ WebSocket ws
      { getDatumByHash: mkListenerSet getDatumByHashDispatchMap
          getDatumByHashPendingRequests
      , getDatumsByHashes: mkListenerSet getDatumsByHashesDispatchMap
          getDatumsByHashesPendingRequests
      , startFetchBlocks: mkListenerSet startFetchBlocksDispatchMap
          startFetchBlocksPendingRequests
      , cancelFetchBlocks: mkListenerSet cancelFetchBlocksDispatchMap
          cancelFetchBlocksPendingRequests
      }
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err
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
  , evaluate :: ListenerSet { txCbor :: ByteArray } Ogmios.TxEvaluationResult
  }

type DatumCacheListeners =
  { getDatumByHash :: ListenerSet DatumHash GetDatumByHashR
  , getDatumsByHashes :: ListenerSet (Array DatumHash) GetDatumsByHashesR
  , startFetchBlocks ::
      ListenerSet { slot :: Slot, id :: Chain.BlockHeaderHash }
        StartFetchBlocksR
  , cancelFetchBlocks :: ListenerSet Unit CancelFetchBlocksR
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

-- | Builds a Ogmios request action using QueryM
mkOgmiosRequest
  :: forall (request :: Type) (response :: Type)
   . JsonWsp.JsonWspCall request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkOgmiosRequest = mkRequest
  (listeners <<< _.ogmiosWs <$> ask)
  (underlyingWebSocket <<< _.ogmiosWs <$> ask)

mkDatumCacheRequest
  :: forall (request :: Type) (response :: Type)
   . JsonWsp.JsonWspCall request response
  -> (DatumCacheListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkDatumCacheRequest = mkRequest
  (listeners <<< _.datumCacheWs <$> ask)
  (underlyingWebSocket <<< _.datumCacheWs <$> ask)

-- | Builds a Ogmios request action using QueryM
mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . QueryM listeners
  -> QueryM JsWebSocket
  -> JsonWsp.JsonWspCall request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkRequest getListeners getWebSocket jsonWspCall getLs inp = do
  { body, id } <- liftEffect $ JsonWsp.buildRequest jsonWspCall inp
  config <- ask
  ws <- getWebSocket
  respLs <- getLs <$> getListeners
  let
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
      _wsSend ws (logString config.logLevel Debug) sBody
      pure $ Canceler $ \err -> do
        liftEffect $ respLs.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc

-------------------------------------------------------------------------------
-- Dispatch Setup
--------------------------------------------------------------------------------

data DispatchError = JsError Error | JsonError JsonDecodeError

dispatchErrorToError :: DispatchError -> Error
dispatchErrorToError (JsError err) = err
dispatchErrorToError (JsonError err) = error $ show err

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
     , evaluateTxDispatchMap :: DispatchIdMap Ogmios.TxEvaluationResult
     , submitDispatchMap :: DispatchIdMap Ogmios.SubmitTxR
     }
  -> Array WebsocketDispatch
ogmiosMessageDispatch
  { utxoDispatchMap
  , chainTipDispatchMap
  , evaluateTxDispatchMap
  , submitDispatchMap
  } =
  [ queryDispatch utxoDispatchMap
  , queryDispatch chainTipDispatchMap
  , queryDispatch evaluateTxDispatchMap
  , queryDispatch submitDispatchMap
  ]

datumCacheMessageDispatch
  :: { getDatumByHashDispatchMap :: DispatchIdMap GetDatumByHashR
     , getDatumsByHashesDispatchMap :: DispatchIdMap GetDatumsByHashesR
     , startFetchBlocksDispatchMap :: DispatchIdMap StartFetchBlocksR
     , cancelFetchBlocksDispatchMap :: DispatchIdMap CancelFetchBlocksR
     }
  -> Array WebsocketDispatch
datumCacheMessageDispatch
  { getDatumByHashDispatchMap
  , getDatumsByHashesDispatchMap
  , startFetchBlocksDispatchMap
  , cancelFetchBlocksDispatchMap
  } =
  [ queryDispatch getDatumByHashDispatchMap
  , queryDispatch getDatumsByHashesDispatchMap
  , queryDispatch startFetchBlocksDispatchMap
  , queryDispatch cancelFetchBlocksDispatchMap
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
  => DispatchIdMap response
  -> String
  -> Effect (Either DispatchError (Effect Unit))
queryDispatch ref str = do
  -- Parse response
  case JsonWsp.parseJsonWspResponse =<< parseJsonStringToAeson str of
    Left parseError -> do
      -- Try to at least parse ID  to dispatch the error to
      case parseJsonWspResponseId =<< parseJsonStringToAeson str of
        -- We still return original error because ID parse error is useless
        Left _idParseError -> pure $ Left $ JsonError parseError
        Right (id :: ListenerId) -> do
          idMap <- Ref.read ref
          let
            (mAction :: Maybe (Either DispatchError response -> Effect Unit)) =
              MultiMap.lookup id idMap
          case mAction of
            Nothing -> pure $ Left $ JsError $ error $
              "Parse failed and Request Id: " <> id <> " has been cancelled"
            Just action -> pure $ Right $ action $ Left $ JsonError parseError
    Right parsed -> do
      let (id :: ListenerId) = parsed.reflection
      idMap <- Ref.read ref
      let
        (mAction :: Maybe (Either DispatchError response -> Effect Unit)) =
          MultiMap.lookup id idMap
      case mAction of
        Nothing -> pure $ Left $ JsError $ error $
          "Parse succeeded but Request Id: " <> id <> " has been cancelled"
        Just action -> pure $ Right $ action $ Right parsed.result

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
              "unexpected parse error on input: " <> msg

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
