module Ogmios
  ( DispatchIdMap
  , ListenerSet
  , Listeners
  , OgmiosWebSocket(OgmiosWebSocket)
  , QueryConfig
  , QueryM
  , WebSocket
  , mkOgmiosWebSocketAff
  , utxosAt
  , utxosAt'
  ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut as Json
import Data.Bitraversable (bisequence, bitraverse)
import Data.Either (Either(Left, Right), either, isRight)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Effect.Ref as Ref

import Deserialization as Deserialization
import Helpers as Helpers
import Serialization as Serialization
import Serialization (addressPubKeyHash, newAddressFromBech32, newBaseAddressFromAddress)
import Types.ByteArray (hexToByteArray)
import Types.JsonWsp as JsonWsp
import Types.JsonWsp (OgmiosTxOut, JsonWspResponse, mkUtxosAtQuery, parseJsonWspResponse, TxOutRef, UtxoQR(UtxoQR))
import Types.Transaction (Address, TransactionHash, TransactionInput, TransactionOutput, UtxoM)

-- This module defines an Aff interface for Ogmios Websocket Queries
-- Since WebSockets do not define a mechanism for linking request/response
-- Or for verifying that the connection is live, those concerns are addressed
-- here

--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import _mkWebSocket :: Url -> Effect WebSocket

foreign import _onWsConnect :: WebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: WebSocket -> String -> Effect Unit

foreign import _wsClose :: WebSocket -> Effect Unit

foreign import _stringify :: forall a. a -> Effect String

foreign import _wsWatch :: WebSocket -> Effect Unit -> Effect Unit

data WebSocket

type Url = String

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- when we add multiple query backends or wallets,
-- we just need to extend this type
type QueryConfig = { ws :: OgmiosWebSocket }

type QueryM a = ReaderT QueryConfig Aff a

-- the first query type in the QueryM/Aff interface
utxosAt :: JsonWsp.Address -> QueryM UtxoQR
utxosAt addr = do
  body <- liftEffect $ mkUtxosAtQuery { utxo: [ addr ] }
  let id = body.mirror.id
  sBody <- liftEffect $ _stringify body
  config <- ask
  -- not sure there's an easy way to factor this out unfortunately
  let
    affFunc :: (Either Error UtxoQR -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      let
        ls = listeners config.ws
        ws = underlyingWebSocket config.ws
      ls.utxo.addMessageListener id
        ( \result -> do
            ls.utxo.removeMessageListener id
            (allowError cont) $ result
        )
      _wsSend ws sBody
      pure $ Canceler $ \err -> do
        liftEffect $ ls.utxo.removeMessageListener id
        liftEffect $ throwError $ err
  liftAff $ makeAff $ affFunc

allowError :: (Either Error UtxoQR -> Effect Unit) -> UtxoQR -> Effect Unit
allowError func = func <<< Right

--------------------------------------------------------------------------------
-- OgmiosWebSocket Setup and PrimOps
--------------------------------------------------------------------------------

-- don't export this constructor
-- type-safe websocket which has automated req/res dispatch and websocket
-- failure handling
data OgmiosWebSocket = OgmiosWebSocket WebSocket Listeners

-- smart-constructor for OgmiosWebSocket in Aff Context
-- (prevents sending messages before the websocket opens, etc)
mkOgmiosWebSocket'
  :: Url
  -> (Either Error OgmiosWebSocket -> Effect Unit)
  -> Effect Canceler
mkOgmiosWebSocket' url cb = do
  utxoQueryDispatchIdMap <- createMutableDispatch
  let md = (messageDispatch utxoQueryDispatchIdMap)
  ws <- _mkWebSocket url
  _onWsConnect ws $ do
    _wsWatch ws (removeAllListeners utxoQueryDispatchIdMap)
    _onWsMessage ws (defaultMessageListener md)
    _onWsError ws defaultErrorListener
    cb $ Right $ OgmiosWebSocket ws { utxo: mkListenerSet utxoQueryDispatchIdMap }
  pure $ Canceler $ \err -> liftEffect $ cb $ Left $ err

-- makeAff
-- :: forall a
-- . ((Either Error a -> Effect Unit) -> Effect Canceler)
-- -> Aff a

mkOgmiosWebSocketAff :: Url -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff url = makeAff (mkOgmiosWebSocket' url)

-- getter
underlyingWebSocket :: OgmiosWebSocket -> WebSocket
underlyingWebSocket (OgmiosWebSocket ws _) = ws

-- getter
listeners :: OgmiosWebSocket -> Listeners
listeners (OgmiosWebSocket _ ls) = ls

-- interface required for adding/removing listeners
type Listeners =
  { utxo :: ListenerSet UtxoQR
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
        Ref.write (Map.insert id func idMap) dim
  , removeMessageListener:
      \id -> do
        idMap <- Ref.read dim
        Ref.write (Map.delete id idMap) dim
  , dispatchIdMap: dim
  }

removeAllListeners :: DispatchIdMap UtxoQR -> Effect Unit
removeAllListeners dim = do
  log "error hit, removing all listeners"
  Ref.write Map.empty dim

-------------------------------------------------------------------------------
-- Dispatch Setup
--------------------------------------------------------------------------------

-- A function which accepts some unparsed Json, and checks it against one or
-- more possible types to perform an appropriate effect (such as supplying the
-- parsed result to an async fiber/Aff listener)
type WebsocketDispatch = String -> Effect (Either Json.JsonDecodeError (Effect Unit))

-- A mutable queue of requests
type DispatchIdMap a = Ref.Ref (Map.Map String (a -> Effect Unit))

-- an immutable queue of response type handlers
messageDispatch :: DispatchIdMap UtxoQR -> Array WebsocketDispatch
messageDispatch dim =
  [ utxoQueryDispatch dim
  ]

-- each query type will have a corresponding ref that lives in ReaderT config or similar
-- for utxoQueryDispatch, the `a` parameter will be `UtxoQR` or similar
-- the add and remove listener functions will know to grab the correct mutable dispatch, if one exists.
createMutableDispatch :: forall a. Effect (DispatchIdMap a)
createMutableDispatch = Ref.new Map.empty

-- we parse out the utxo query result, then check if we're expecting a result
-- with the provided id, if we are then we dispatch to the effect that is
-- waiting on this result
utxoQueryDispatch
  :: Ref.Ref (Map.Map String (UtxoQR -> Effect Unit))
  -> String
  -> Effect (Either Json.JsonDecodeError (Effect Unit))
utxoQueryDispatch ref str = do
  let parsed' = parseJsonWspResponse =<< Helpers.parseJsonStringifyNumbers str
  case parsed' of
    (Left err) -> pure $ Left err
    (Right res) -> afterParse res
  where
  afterParse
    :: JsonWspResponse UtxoQR
    -> Effect (Either Json.JsonDecodeError (Effect Unit))
  afterParse parsed = do
    let (id :: String) = parsed.reflection.id
    idMap <- Ref.read ref
    let
      (mAction :: Maybe (UtxoQR -> Effect Unit)) = (Map.lookup id idMap)
    case mAction of
      Nothing -> pure $ (Left (Json.TypeMismatch ("Parse succeeded but Request Id: " <> id <> " has been cancelled")) :: Either Json.JsonDecodeError (Effect Unit))
      Just action -> pure $ Right $ action parsed.result

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: Json.JsonDecodeError
defaultErr = Json.TypeMismatch "default error"

-- For now, we just throw this error, if we find error types that can be linked
-- to request Id's, then we should run a similar dispatch and throw within the
-- appropriate Aff handler
defaultErrorListener :: String -> Effect Unit
defaultErrorListener str =
  throwError $ error $ "a WebSocket Error has occured: " <> str

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
-- | Converts an JsonWsp.Address to (internal) Address
ogmiosAddressToAddress :: JsonWsp.Address -> Aff (Maybe Address)
ogmiosAddressToAddress ogAddr =
  newAddressFromBech32 (wrap ogAddr) <#> Deserialization.convertAddress
    # liftEffect

-- | Converts an (internal) Address to JsonWsp.Address
addressToOgmiosAddress :: Address -> Aff (Maybe JsonWsp.Address)
addressToOgmiosAddress addr = do
  addr' <- liftEffect $ Serialization.convertAddress addr
  pure $ newBaseAddressFromAddress addr' >>= addressPubKeyHash <#> unwrap

-- If required, we can change to Either with more granular error handling.
-- | Gets utxos at an (internal) Address in terms of (internal) Transaction.Types
utxosAt' :: Address -> QueryM (Maybe UtxoM)
utxosAt' addr = do
  addr' :: Maybe JsonWsp.Address <- lift $ addressToOgmiosAddress addr
  maybe (pure Nothing) getUtxos addr'
  where
  getUtxos :: JsonWsp.Address -> QueryM (Maybe UtxoM)
  getUtxos address = convertUtxos >>> lift =<< utxosAt address

  convertUtxos :: UtxoQR -> Aff (Maybe UtxoM)
  convertUtxos (UtxoQR utxoQueryResult) = do
    out' :: Array (Maybe TransactionInput /\ Maybe TransactionOutput) <-
      Map.toUnfoldable utxoQueryResult
        <#> bitraverse
          txOutRefToTransactionInput
          ogmiosTxOutToTransactionOutput
        # sequence
    let
      out :: Maybe (Array (TransactionInput /\ TransactionOutput))
      out = out' <#> bisequence # sequence
    pure $ maybe Nothing (pure <<< wrap <<< Map.fromFoldable) out

-- I think txId is a hexadecimal encoding.
-- | Converts an Ogmios TxOutRef to (internal) TransactionInput
txOutRefToTransactionInput :: TxOutRef -> Aff (Maybe TransactionInput)
txOutRefToTransactionInput { txId, index } = do
  transaction_id' :: Maybe TransactionHash <-
    hexToByteArray txId <#> wrap # pure
  pure $ case transaction_id' of
    Nothing -> Nothing
    Just transaction_id ->
      pure $ wrap
        { transaction_id
        , index
        }

-- https://ogmios.dev/ogmios.wsp.json see "datum", potential FIX ME: it says
-- base64 but the  example provided looks like a hexadecimal so use
-- hexToByteArray for now.
-- | Converts an Ogmios TxOut to (internal) TransactionOutput
ogmiosTxOutToTransactionOutput :: OgmiosTxOut -> Aff (Maybe TransactionOutput)
ogmiosTxOutToTransactionOutput { address: address'', value, datum } = do
  address' :: Maybe Address <- ogmiosAddressToAddress address''
  pure $ case address' of
    Nothing -> Nothing
    Just address ->
      pure $ wrap
        { address
        , amount: value
        , data_hash: datum >>= hexToByteArray <#> wrap
        }