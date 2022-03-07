module QueryM
  ( DispatchIdMap
  , FeeEstimate(FeeEstimate)
  , FeeEstimateError(FeeEstimateHttpError, FeeEstimateDecodeJsonError)
  , Host
  , ListenerSet
  , OgmiosListeners
  , DatumCacheListeners
  , WebSocket
  , OgmiosWebSocket
  , DatumCacheWebSocket
  , QueryConfig
  , QueryM
  , ServerConfig
  , JsWebSocket
  , addressToOgmiosAddress
  , calculateMinFee
  , defaultServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , getWalletAddress
  , getWalletCollateral
  , mkOgmiosWebSocketAff
  , mkDatumCacheWebSocketAff
  , mkHttpUrl
  , mkWsUrl
  , ogmiosAddressToAddress
  , signTransaction
  , submitTransaction
  , utxosAt
  , queryDatumCache
  , getDatumByHash
  , getDatumsByHashes
  , startFetchBlocksRequest
  , cancelFetchBlocksRequest
  , datumFilterAddHashesRequest
  , datumFilterRemoveHashesRequest
  , datumFilterSetHashesRequest
  , datumFilterGetHashesRequest
  ) where

import Prelude

import Aeson (decodeAeson, parseJsonStringToAeson)
import Affjax as Affjax
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask, asks)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut as Json
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Bitraversable (bisequence)
import Data.Either (Either(Left, Right), either, isRight, note)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import DatumCacheWsp
  ( DatumCacheMethod
      ( StartFetchBlocks
      , CancelFetchBlocks
      , DatumFilterAddHashes
      , DatumFilterRemoveHashes
      , DatumFilterSetHashes
      )
  , DatumCacheRequest
      ( GetDatumByHashRequest
      , GetDatumsByHashesRequest
      , StartFetchBlocksRequest
      , CancelFetchBlocksRequest
      , DatumFilterAddHashesRequest
      , DatumFilterRemoveHashesRequest
      , DatumFilterSetHashesRequest
      , DatumFilterGetHashesRequest
      )
  , DatumCacheResponse
      ( GetDatumByHashResponse
      , GetDatumsByHashesResponse
      , DatumFilterGetHashesResponse
      )
  , faultToString
  , requestMethodName
  , responseMethod
  )
import DatumCacheWsp as DcWsp
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Helpers as Helpers
import MultiMap (MultiMap)
import MultiMap as MM
import Serialization as Serialization
import Serialization.Address
  ( Address
  , BlockId
  , Slot
  , addressBech32
  , addressFromBech32
  )
import Types.ByteArray
  ( hexToByteArray
  , byteArrayToHex
  )
import Types.JsonWsp as JsonWsp
import Types.PlutusData (DatumHash, PlutusData)
import Types.Transaction as Transaction
import Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Types.Value (Coin(Coin))
import Untagged.Union (asOneOf)
import Wallet (Wallet(Nami), NamiWallet, NamiConnection)

-- This module defines an Aff interface for Ogmios Websocket Queries
-- Since WebSockets do not define a mechanism for linking request/response
-- Or for verifying that the connection is live, those concerns are addressed
-- here

--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import _mkWebSocket :: Url -> Effect JsWebSocket

foreign import _onWsConnect :: JsWebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: JsWebSocket -> String -> Effect Unit

foreign import _wsClose :: JsWebSocket -> Effect Unit

foreign import _stringify :: forall (a :: Type). a -> Effect String

foreign import _wsWatch :: JsWebSocket -> Effect Unit -> Effect Unit

foreign import data JsWebSocket :: Type

type Url = String

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- when we add multiple query backends or wallets,
-- we just need to extend this type
type QueryConfig =
  { ogmiosWs :: OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , serverConfig :: ServerConfig
  , wallet :: Maybe Wallet
  }

type QueryM (a :: Type) = ReaderT QueryConfig Aff a

-- the first query type in the QueryM/Aff interface
utxosAt' :: JsonWsp.Address -> QueryM JsonWsp.UtxoQR
utxosAt' addr = do
  body <- liftEffect $ JsonWsp.mkUtxosAtQuery { utxo: [ addr ] }
  let id = body.mirror.id
  sBody <- liftEffect $ _stringify body
  config <- ask
  -- not sure there's an easy way to factor this out unfortunately
  let
    affFunc :: (Either Error JsonWsp.UtxoQR -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners config.ogmiosWs
        ws = underlyingWebSocket config.ogmiosWs
      ls.utxo.addMessageListener id
        ( \result -> do
            ls.utxo.removeMessageListener id
            allowError cont $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.utxo.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc

--------------------------------------------------------------------------------
-- Datum Cache Queries
--------------------------------------------------------------------------------

getDatumByHash :: DatumHash -> QueryM (Maybe PlutusData)
getDatumByHash hash = do
  queryDatumCache (GetDatumByHashRequest hash) >>= case _ of
    GetDatumByHashResponse mData -> pure mData
    _ -> liftEffect $ throw "Request-response type mismatch. Should not have happened"

getDatumsByHashes :: Array DatumHash -> QueryM (Array PlutusData)
getDatumsByHashes hashes = do
  queryDatumCache (GetDatumsByHashesRequest hashes) >>= case _ of
    GetDatumsByHashesResponse plutusDatums -> pure $ plutusDatums
    _ -> liftEffect $ throw "Request-response type mismatch. Should not have happened"

startFetchBlocksRequest :: { slot :: Slot, id :: BlockId } -> QueryM Unit
startFetchBlocksRequest = matchCacheQuery StartFetchBlocksRequest StartFetchBlocks

-- | Cancels a running block fetcher job. Throws on no fetchers running
cancelFetchBlocksRequest :: QueryM Unit
cancelFetchBlocksRequest = matchCacheQuery (const CancelFetchBlocksRequest) CancelFetchBlocks unit

datumFilterAddHashesRequest :: Array DatumHash -> QueryM Unit
datumFilterAddHashesRequest = matchCacheQuery DatumFilterAddHashesRequest DatumFilterAddHashes

datumFilterRemoveHashesRequest :: Array DatumHash -> QueryM Unit
datumFilterRemoveHashesRequest = matchCacheQuery DatumFilterRemoveHashesRequest DatumFilterRemoveHashes

datumFilterSetHashesRequest :: Array DatumHash -> QueryM Unit
datumFilterSetHashesRequest = matchCacheQuery DatumFilterSetHashesRequest DatumFilterSetHashes

datumFilterGetHashesRequest :: QueryM (Array DatumHash)
datumFilterGetHashesRequest = do
  queryDatumCache DatumFilterGetHashesRequest >>= case _ of
    DatumFilterGetHashesResponse hashes -> pure $ hashes
    _ -> liftEffect $ throw "Request-response type mismatch. Should not have happened"

matchCacheQuery
  :: forall (args :: Type)
   . (args -> DatumCacheRequest)
  -> DatumCacheMethod
  -> args
  -> QueryM Unit
matchCacheQuery query method args = do
  resp <- queryDatumCache (query args)
  if responseMethod resp == method then pure unit
  else liftEffect $ throw "Request-response type mismatch. Should not have happened"

queryDatumCache :: DatumCacheRequest -> QueryM DatumCacheResponse
queryDatumCache request = do
  sBody <- liftEffect $ _stringify $ DcWsp.jsonWspRequest request
  config <- ask
  let
    id = requestMethodName request

    affFunc :: (Either Error DcWsp.JsonWspResponse -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners config.datumCacheWs
        ws = underlyingWebSocket config.datumCacheWs
      ls.addMessageListener id
        ( \result -> do
            ls.removeMessageListener id
            allowError cont $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.removeMessageListener id
        liftEffect $ throwError $ err
  jsonwspresp <- liftAff $ makeAff $ affFunc
  case DcWsp.parseJsonWspResponse jsonwspresp of
    Right resp -> pure resp
    Left fault -> liftEffect $ throw $ "Ogmios-datum-cache service call fault" <> faultToString fault

allowError :: forall (a :: Type). (Either Error a -> Effect Unit) -> a -> Effect Unit
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

submitTransaction
  :: Transaction.Transaction -> QueryM (Maybe Transaction.TransactionHash)
submitTransaction tx = withMWalletAff $ case _ of
  Nami nami -> callNami nami $ \nw -> flip nw.submitTx tx

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

-- WS/HTTP server config
--------------------------------------------------------------------------------

type ServerConfig =
  { port :: UInt
  , host :: Host
  , secure :: Boolean
  }

defaultServerConfig :: ServerConfig
defaultServerConfig =
  { port: UInt.fromInt 8081
  , host: "localhost"
  , secure: false
  }

defaultOgmiosWsConfig :: ServerConfig
defaultOgmiosWsConfig =
  { port: UInt.fromInt 1337
  , host: "localhost"
  , secure: false
  }

defaultDatumCacheWsConfig :: ServerConfig
defaultDatumCacheWsConfig =
  { port: UInt.fromInt 9999
  , host: "localhost"
  , secure: false
  }

type Host = String

mkHttpUrl :: ServerConfig -> Url
mkHttpUrl = mkServerUrl "http"

mkWsUrl :: ServerConfig -> Url
mkWsUrl = mkServerUrl "ws"

mkServerUrl :: String -> ServerConfig -> Url
mkServerUrl protocol cfg =
  (if cfg.secure then (protocol <> "s") else protocol)
    <> "://"
    <> cfg.host
    <> ":"
    <> UInt.toString cfg.port

-- The server will respond with a stringified integer value for the fee estimate
newtype FeeEstimate = FeeEstimate BigInt

derive instance Newtype FeeEstimate _

instance Json.DecodeJson FeeEstimate where
  decodeJson str =
    map FeeEstimate
      <<< note (Json.TypeMismatch "Expected a `BigInt`")
      <<< BigInt.fromString
      =<< Json.caseJsonString
        (Left $ Json.TypeMismatch "Expected a stringified `BigInt`")
        Right
        str

data FeeEstimateError
  = FeeEstimateHttpError Affjax.Error
  | FeeEstimateDecodeJsonError Json.JsonDecodeError

-- No Show instance of Affjax.Error
instance Show FeeEstimateError where
  show (FeeEstimateHttpError err) =
    "(FeeEstimateHttpError "
      <> Affjax.printError err
      <> ")"
  show (FeeEstimateDecodeJsonError err) =
    "(FeeEstimateDecodeJsonError "
      <> show err
      <> ")"

-- Query the Haskell server for the minimum transaction fee
calculateMinFee
  :: Transaction.Transaction -> QueryM (Either FeeEstimateError Coin)
calculateMinFee tx = do
  url <- asks $ mkHttpUrl <<< _.serverConfig
  txHex <- liftEffect $
    byteArrayToHex
      <<< Serialization.toBytes
      <<< asOneOf
      <$> Serialization.convertTransaction tx
  liftAff (Affjax.get Affjax.ResponseFormat.json $ url <> "/fees?tx=" <> txHex)
    <#> case _ of
      Left e -> Left $ FeeEstimateHttpError e
      Right resp ->
        bimap
          FeeEstimateDecodeJsonError
          -- FIXME
          -- Add some "padding" to the fees so the transaction will submit
          -- The server is calculating fees that are too low
          -- See https://github.com/Plutonomicon/cardano-browser-tx/issues/123
          (Coin <<< ((+) (BigInt.fromInt 50000)) <<< (unwrap :: FeeEstimate -> BigInt))
          $ Json.decodeJson resp.body

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
  :: ServerConfig
  -> (Either Error OgmiosWebSocket -> Effect Unit)
  -> Effect Canceler
mkOgmiosWebSocket' serverCfg cb = do
  utxoQueryDispatchIdMap <- createMutableDispatch
  let md = ogmiosMessageDispatch utxoQueryDispatchIdMap
  ws <- _mkWebSocket $ mkWsUrl serverCfg
  _onWsConnect ws $ do
    _wsWatch ws (removeAllListeners utxoQueryDispatchIdMap)
    _onWsMessage ws (defaultMessageListener md)
    _onWsError ws defaultErrorListener
    cb $ Right $ WebSocket ws { utxo: mkListenerSet utxoQueryDispatchIdMap }
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err

mkDatumCacheWebSocket'
  :: ServerConfig
  -> (Either Error DatumCacheWebSocket -> Effect Unit)
  -> Effect Canceler
mkDatumCacheWebSocket' serverCfg cb = do
  dispatchMap <- createMutableDispatch
  let md = (datumCacheMessageDispatch dispatchMap)
  ws <- _mkWebSocket $ mkWsUrl serverCfg
  _onWsConnect ws $ do
    _wsWatch ws (removeAllListeners dispatchMap)
    _onWsMessage ws (defaultMessageListener md)
    _onWsError ws defaultErrorListener
    cb $ Right $ WebSocket ws (mkListenerSet dispatchMap)
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err

-- makeAff
-- :: forall a
-- . ((Either Error a -> Effect Unit) -> Effect Canceler)
-- -> Aff a
mkDatumCacheWebSocketAff :: ServerConfig -> Aff DatumCacheWebSocket
mkDatumCacheWebSocketAff serverCfg = makeAff $ mkDatumCacheWebSocket' serverCfg

mkOgmiosWebSocketAff :: ServerConfig -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff serverCfg = makeAff $ mkOgmiosWebSocket' serverCfg

-- getter
underlyingWebSocket :: forall a. WebSocket a -> JsWebSocket
underlyingWebSocket (WebSocket ws _) = ws

-- getter
listeners :: forall listeners. WebSocket listeners -> listeners
listeners (WebSocket _ ls) = ls

-- interface required for adding/removing listeners
type DatumCacheListeners = ListenerSet DcWsp.JsonWspResponse

type OgmiosListeners =
  { utxo :: ListenerSet JsonWsp.UtxoQR
  }

-- convenience type for adding additional query types later
type ListenerSet a =
  { addMessageListener :: String -> (a -> Effect Unit) -> Effect Unit
  , removeMessageListener :: String -> Effect Unit
  , dispatchIdMap :: DispatchIdMap a
  }

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet :: forall a. DispatchIdMap a -> ListenerSet a
mkListenerSet dim =
  { addMessageListener:
      \id -> \func -> do
        idMap <- Ref.read dim
        Ref.write (MM.insert id func idMap) dim
  , removeMessageListener:
      \id -> do
        idMap <- Ref.read dim
        Ref.write (MM.delete id idMap) dim
  , dispatchIdMap: dim
  }

removeAllListeners :: forall a. DispatchIdMap a -> Effect Unit
removeAllListeners dim = do
  log "error hit, removing all listeners"
  Ref.write MM.empty dim

-------------------------------------------------------------------------------
-- Dispatch Setup
--------------------------------------------------------------------------------

-- A function which accepts some unparsed Json, and checks it against one or
-- more possible types to perform an appropriate effect (such as supplying the
-- parsed result to an async fiber/Aff listener)
type WebsocketDispatch = String -> Effect (Either Json.JsonDecodeError (Effect Unit))

-- A mutable queue of requests
type DispatchIdMap a = Ref.Ref (MultiMap String (a -> Effect Unit))

-- an immutable queue of response type handlers
ogmiosMessageDispatch :: DispatchIdMap JsonWsp.UtxoQR -> Array WebsocketDispatch
ogmiosMessageDispatch dim =
  [ utxoQueryDispatch dim
  ]

datumCacheMessageDispatch :: DispatchIdMap DcWsp.JsonWspResponse -> Array WebsocketDispatch
datumCacheMessageDispatch dim =
  [ datumCacheQueryDispatch dim ]

-- each query type will have a corresponding ref that lives in ReaderT config or similar
-- for utxoQueryDispatch, the `a` parameter will be `UtxoQR` or similar
-- the add and remove listener functions will know to grab the correct mutable dispatch, if one exists.
createMutableDispatch :: forall a. Effect (DispatchIdMap a)
createMutableDispatch = Ref.new MM.empty

-- we parse out the utxo query result, then check if we're expecting a result
-- with the provided id, if we are then we dispatch to the effect that is
-- waiting on this result
utxoQueryDispatch
  :: Ref.Ref (MultiMap String (JsonWsp.UtxoQR -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
utxoQueryDispatch ref str = do
  -- TODO: replace it with the new implementation in `Aeson`.
  -- https://github.com/Plutonomicon/cardano-browser-tx/issues/151
  let parsed' = JsonWsp.parseJsonWspResponse =<< Helpers.parseJsonStringifyNumbers str
  case parsed' of
    (Left err) -> pure $ Left err
    (Right res) -> afterParse res
  where
  afterParse
    :: JsonWsp.JsonWspResponse JsonWsp.UtxoQR
    -> Effect (Either Json.JsonDecodeError (Effect Unit))
  afterParse parsed = do
    let (id :: String) = parsed.reflection.id
    idMap <- Ref.read ref
    let
      (mAction :: Maybe (JsonWsp.UtxoQR -> Effect Unit)) = (MM.lookup id idMap)
    case mAction of
      Nothing -> pure $ (Left (Json.TypeMismatch ("Parse succeeded but Request Id: " <> id <> " has been cancelled")) :: Either Json.JsonDecodeError (Effect Unit))
      Just action -> pure $ Right $ action parsed.result

datumCacheQueryDispatch
  :: Ref.Ref (MultiMap String (DcWsp.JsonWspResponse -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
datumCacheQueryDispatch dim str = do
  case parse str of
    (Left err) -> pure $ Left err
    (Right res) -> afterParse res
  where
  parse :: String -> Either JsonDecodeError DcWsp.JsonWspResponse
  parse = parseJsonStringToAeson >=> decodeAeson

  afterParse
    :: DcWsp.JsonWspResponse
    -> Effect (Either Json.JsonDecodeError (Effect Unit))
  afterParse parsed = do
    idMap <- Ref.read dim
    let id = parsed.methodname
    case MM.lookup id idMap of
      Nothing -> pure $ (Left (Json.TypeMismatch ("Parse succeeded but Request Id: " <> id <> " has been cancelled")) :: Either Json.JsonDecodeError (Effect Unit))
      Just action -> pure $ Right $ action parsed

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: Json.JsonDecodeError
defaultErr = Json.TypeMismatch "default error"

-- For now, we just throw this error, if we find error types that can be linked
-- to request Id's, then we should run a similar dispatch and throw within the
-- appropriate Aff handler
defaultErrorListener :: String -> Effect Unit
defaultErrorListener str =
  throwError $ error $ "a JsWebSocket Error has occured: " <> str

defaultMessageListener :: Array WebsocketDispatch -> String -> Effect Unit
defaultMessageListener dispatchArray msg = do
  -- here, we need to fold the input over the array of functions until we get
  -- a success, then execute the effect.
  -- using a fold instead of a traverse allows us to skip a bunch of execution
  eAction :: Either Json.JsonDecodeError (Effect Unit) <- foldl (messageFoldF msg) (pure $ Left defaultErr) dispatchArray
  either
    -- we expect a lot of parse errors, some messages could? fall through completely
    (\err -> if err == defaultErr then pure unit else log ("unexpected parse error on input:" <> msg))
    (\act -> act)
    (eAction :: Either Json.JsonDecodeError (Effect Unit))

messageFoldF
  :: String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
  -> (String -> (Effect (Either Json.JsonDecodeError (Effect Unit))))
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
messageFoldF msg acc' func = do
  acc <- acc'
  if isRight acc then acc' else func msg

--------------------------------------------------------------------------------
-- Ogmios functions and types to internal types
--------------------------------------------------------------------------------
-- JsonWsp.Address is a bech32 string, so wrap to Transaction.Types.Bech32
-- | Converts an `JsonWsp.Address` (bech32string) to Address
ogmiosAddressToAddress :: JsonWsp.Address -> Maybe Address
ogmiosAddressToAddress = addressFromBech32

-- | Converts an (internal) Address to `JsonWsp.Address` (bech32string)
addressToOgmiosAddress :: Address -> JsonWsp.Address
addressToOgmiosAddress = addressBech32

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) `Address` in terms of (internal) `Transaction.Types`.
-- Results may vary depending on `Wallet` type.
utxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
utxosAt addr = asks _.wallet >>= maybe (pure Nothing) (utxosAtByWallet addr)
  where
  -- Add more wallet types here:
  utxosAtByWallet
    :: Address -> Wallet -> QueryM (Maybe Transaction.UtxoM)
  utxosAtByWallet address (Nami _) = namiUtxosAt address
  -- Unreachable but helps build when we add wallets, most of them shouldn't
  -- require any specific behaviour.
  utxosAtByWallet address _ = allUtxosAt address

  -- Gets all utxos at an (internal) Address in terms of (internal)
  -- Transaction.Types.
  allUtxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
  allUtxosAt = addressToOgmiosAddress >>> getUtxos
    where
    getUtxos :: JsonWsp.Address -> QueryM (Maybe Transaction.UtxoM)
    getUtxos address = convertUtxos <$> utxosAt' address

    convertUtxos :: JsonWsp.UtxoQR -> Maybe Transaction.UtxoM
    convertUtxos (JsonWsp.UtxoQR utxoQueryResult) =
      let
        out' :: Array (Maybe Transaction.TransactionInput /\ Maybe Transaction.TransactionOutput)
        out' = Map.toUnfoldable utxoQueryResult
          <#> bimap
            txOutRefToTransactionInput
            ogmiosTxOutToTransactionOutput

        out :: Maybe (Array (Transaction.TransactionInput /\ Transaction.TransactionOutput))
        out = out' <#> bisequence # sequence
      in
        (wrap <<< Map.fromFoldable) <$> out

  -- Nami appear to remove collateral from the utxo set, so we shall do the same.
  -- This is crucial if we are submitting via Nami. If we decide to submit with
  -- Ogmios, we can remove this.
  -- More detail can be found here https://github.com/Berry-Pool/nami-wallet/blob/ecb32e39173b28d4a7a85b279a748184d4759f6f/src/api/extension/index.js
  -- by searching "// exclude collateral input from overall utxo set"
  -- or functions getUtxos and checkCollateral.
  namiUtxosAt :: Address -> QueryM (Maybe Transaction.UtxoM)
  namiUtxosAt address = do
    utxos' <- allUtxosAt address
    collateral' <- getWalletCollateral
    pure do
      utxos <- unwrap <$> utxos'
      collateral <- unwrap <$> collateral'
      pure $ wrap $ Map.delete collateral.input utxos

-- I think txId is a hexadecimal encoding.
-- | Converts an Ogmios `TxOutRef` to (internal) `TransactionInput`
txOutRefToTransactionInput :: JsonWsp.TxOutRef -> Maybe Transaction.TransactionInput
txOutRefToTransactionInput { txId, index } = do
  transaction_id <- hexToByteArray txId <#> wrap
  pure $ wrap
    { transaction_id
    , index
    }

-- https://ogmios.dev/ogmios.wsp.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now.
-- | Converts an Ogmios `TxOut` to (internal) `TransactionOutput`
ogmiosTxOutToTransactionOutput
  :: JsonWsp.OgmiosTxOut
  -> Maybe Transaction.TransactionOutput
ogmiosTxOutToTransactionOutput { address, value, datum } = do
  address' <- ogmiosAddressToAddress address
  pure $ wrap
    { address: address'
    , amount: value
    , data_hash: datum >>= hexToByteArray <#> wrap
    }
