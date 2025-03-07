module Ctl.Internal.QueryM.Ogmios.Mempool
  ( ReleasedMempool(ReleasedMempool)
  , MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , HasTxR(HasTxR)
  , MaybeMempoolTransaction(MaybeMempoolTransaction)
  , acquireMempoolSnapshotCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , releaseMempoolCall
  , ListenerSet
  , OgmiosListeners
  , ListenerId
  , mkOgmiosCallType
  , OgmiosWebSocket
  , WebSocket(WebSocket)
  , listeners
  , mkListenerSet
  , defaultMessageListener
  , mkOgmiosWebSocketAff
  , mkRequestAff
  , underlyingWebSocket
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , getField
  , parseJsonStringToAeson
  , stringifyAeson
  , (.:)
  )
import Cardano.Provider.TxEvaluation (OgmiosTxId)
import Cardano.Types.Slot (Slot)
import Cardano.Types.TransactionHash (TransactionHash)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, throwError)
import Ctl.Internal.QueryM.Ogmios.Mempool.Dispatcher
  ( DispatchError(JsonError)
  , Dispatcher
  , RequestBody
  , WebsocketDispatch
  , mkWebsocketDispatch
  , newDispatcher
  )
import Ctl.Internal.QueryM.Ogmios.Mempool.JsWebSocket
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
import Ctl.Internal.QueryM.Ogmios.Mempool.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , mkCallType
  )
import Ctl.Internal.QueryM.Ogmios.Mempool.JsonRpc2 as JsonRpc2
import Ctl.Internal.QueryM.Ogmios.Types
  ( class DecodeOgmios
  , OgmiosDecodeError
  , decodeOgmios
  , decodeResult
  , ogmiosDecodeErrorToError
  )
import Data.Argonaut.Encode.Encoders as Argonaut
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, isRight)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel(Error, Debug))
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Foreign.Object (Object)

type ListenerId = String

type Logger = LogLevel -> String -> Effect Unit

--------------------------------------------------------------------------------
-- Ogmios Local Tx Monitor Protocol
--------------------------------------------------------------------------------

acquireMempoolSnapshotCall
  :: JsonRpc2Call Unit MempoolSnapshotAcquired
acquireMempoolSnapshotCall =
  mkOgmiosCallTypeNoArgs "acquireMempool"

mempoolSnapshotHasTxCall
  :: MempoolSnapshotAcquired
  -> JsonRpc2Call TransactionHash HasTxR
mempoolSnapshotHasTxCall _ = mkOgmiosCallType
  { method: "hasTransaction"
  , params: { id: _ }
  }

mempoolSnapshotNextTxCall
  :: MempoolSnapshotAcquired
  -> JsonRpc2Call Unit MaybeMempoolTransaction
mempoolSnapshotNextTxCall _ = mkOgmiosCallType
  { method: "nextTransaction"
  , params: const { fields: "all" }
  }

mempoolSnapshotSizeAndCapacityCall
  :: MempoolSnapshotAcquired
  -> JsonRpc2Call Unit MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityCall _ =
  mkOgmiosCallTypeNoArgs "sizeOfMempool"

releaseMempoolCall
  :: MempoolSnapshotAcquired -> JsonRpc2Call Unit ReleasedMempool
releaseMempoolCall _ =
  mkOgmiosCallTypeNoArgs "releaseMempool"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkOgmiosCallTypeNoArgs
  :: forall (o :: Type)
   . DecodeOgmios o
  => String
  -> JsonRpc2Call Unit o
mkOgmiosCallTypeNoArgs method =
  mkOgmiosCallType { method, params: const {} }

mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => DecodeOgmios o
  => { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkOgmiosCallType =
  mkCallType { jsonrpc: "2.0" }

--------------------------------------------------------------------------------
-- WebSocket
--------------------------------------------------------------------------------

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

mkOgmiosWebSocketAff
  :: Logger
  -> String
  -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff logger serverUrl = do
  lens <- liftEffect $ mkOgmiosWebSocketLens logger
  makeAff $ mkServiceWebSocket lens serverUrl

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
-- `MkServiceWebSocketLens` for ogmios
--------------------------------------------------------------------------------

type MkServiceWebSocketLens (listeners :: Type) =
  { serviceName :: String
  , dispatcher :: Dispatcher
  , logger :: Logger
  , typedWebSocket :: JsWebSocket -> WebSocket listeners
  }

mkOgmiosWebSocketLens
  :: Logger
  -> Effect (MkServiceWebSocketLens OgmiosListeners)
mkOgmiosWebSocketLens logger = do
  dispatcher <- newDispatcher
  pure $
    let
      ogmiosWebSocket :: JsWebSocket -> OgmiosWebSocket
      ogmiosWebSocket ws = WebSocket ws
        { acquireMempool:
            mkListenerSet dispatcher
        , releaseMempool:
            mkListenerSet dispatcher
        , mempoolHasTx:
            mkListenerSet dispatcher
        , mempoolNextTx:
            mkListenerSet dispatcher
        , mempoolSizeAndCapacity:
            mkListenerSet dispatcher
        }

    in
      { serviceName: "ogmios"
      , dispatcher
      , logger
      , typedWebSocket: ogmiosWebSocket
      }

--------------------------------------------------------------------------------
-- ListenerSet
--------------------------------------------------------------------------------

type OgmiosListeners =
  { acquireMempool :: ListenerSet Unit MempoolSnapshotAcquired
  , releaseMempool :: ListenerSet Unit ReleasedMempool
  , mempoolHasTx :: ListenerSet TransactionHash HasTxR
  , mempoolNextTx :: ListenerSet Unit MaybeMempoolTransaction
  , mempoolSizeAndCapacity :: ListenerSet Unit MempoolSizeAndCapacity
  }

-- convenience type for adding additional query types later
type ListenerSet (request :: Type) (response :: Type) =
  { addMessageListener ::
      ListenerId
      -> (Either OgmiosDecodeError response -> Effect Unit)
      -> Effect Unit
  , removeMessageListener :: ListenerId -> Effect Unit
  -- ^ Removes ID from dispatch map and pending requests queue.
  }

mkAddMessageListener
  :: forall (response :: Type)
   . DecodeOgmios response
  => Dispatcher
  -> ( ListenerId
       -> (Either OgmiosDecodeError response -> Effect Unit)
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
  -> (ListenerId -> Effect Unit)
mkRemoveMessageListener dispatcher =
  \reflection -> do
    Ref.modify_ (Map.delete reflection) dispatcher

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet
  :: forall (request :: Type) (response :: Type)
   . DecodeOgmios response
  => Dispatcher
  -> ListenerSet request response
mkListenerSet dispatcher =
  { addMessageListener:
      mkAddMessageListener dispatcher
  , removeMessageListener:
      mkRemoveMessageListener dispatcher
  }

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
      _wsSend webSocket (logger Debug) sBody
      -- Uncomment this code fragment to test `SubmitTx` request resend logic:
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

--------------------------------------------------------------------------------

-- Local Tx Monitor Query Response & Parsing
--------------------------------------------------------------------------------

newtype HasTxR = HasTxR Boolean

derive instance Newtype HasTxR _

instance DecodeOgmios HasTxR where
  decodeOgmios = decodeResult (map HasTxR <<< decodeAeson)

newtype MempoolSnapshotAcquired = AwaitAcquired Slot

instance Show MempoolSnapshotAcquired where
  show (AwaitAcquired slot) = "(AwaitAcquired " <> show slot <> ")"

instance DecodeAeson MempoolSnapshotAcquired where
  decodeAeson =
    -- todo: ignoring "acquired": "mempool"
    map AwaitAcquired <<< aesonObject (flip getField "slot")

instance DecodeOgmios MempoolSnapshotAcquired where
  decodeOgmios = decodeResult decodeAeson

-- | The acquired snapshot’s size (in bytes), number of transactions, and capacity
-- | (in bytes).
newtype MempoolSizeAndCapacity = MempoolSizeAndCapacity
  { capacity :: Prim.Int
  , currentSize :: Prim.Int
  , numberOfTxs :: Prim.Int
  }

derive instance Generic MempoolSizeAndCapacity _
derive instance Newtype MempoolSizeAndCapacity _

instance Show MempoolSizeAndCapacity where
  show = genericShow

instance DecodeAeson MempoolSizeAndCapacity where
  decodeAeson = aesonObject \o -> do
    capacity <- getField o "maxCapacity" >>= flip getField "bytes"
    currentSize <- getField o "currentSize" >>= flip getField "bytes"
    numberOfTxs <- getField o "transactions" >>= flip getField "count"
    pure $ wrap { capacity, currentSize, numberOfTxs }

instance DecodeOgmios MempoolSizeAndCapacity where
  decodeOgmios = decodeResult decodeAeson

newtype MempoolTransaction = MempoolTransaction
  { id :: OgmiosTxId
  , raw :: String -- hex encoded transaction cbor
  }

derive instance Generic MempoolTransaction _
derive instance Newtype MempoolTransaction _

newtype MaybeMempoolTransaction = MaybeMempoolTransaction
  (Maybe MempoolTransaction)

instance DecodeAeson MaybeMempoolTransaction where
  decodeAeson aeson = do
    { transaction: tx } :: { transaction :: Aeson } <- decodeAeson aeson
    res <-
      ( do
          tx' :: { id :: String, cbor :: String } <- decodeAeson tx
          pure $ Just $ MempoolTransaction { id: tx'.id, raw: tx'.cbor }
      ) <|>
        ( do
            caseAesonNull (Left (TypeMismatch "Null")) pure $ tx
            pure Nothing
        )
    pure $ MaybeMempoolTransaction $ res

derive instance Newtype MaybeMempoolTransaction _

instance DecodeOgmios MaybeMempoolTransaction where
  decodeOgmios = decodeResult decodeAeson

data ReleasedMempool = ReleasedMempool

derive instance Generic ReleasedMempool _

instance Show ReleasedMempool where
  show = genericShow

instance DecodeAeson ReleasedMempool where
  decodeAeson = aesonObject \o -> do
    released <- o .: "released"
    flip (caseAesonString (Left (TypeMismatch "String"))) released $ \s ->
      if s == "mempool" then
        pure $ ReleasedMempool
      else
        Left (UnexpectedValue $ Argonaut.encodeString s)

instance DecodeOgmios ReleasedMempool where
  decodeOgmios = decodeResult decodeAeson

aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "Object"))
