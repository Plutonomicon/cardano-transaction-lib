module Ogmios where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

-- foreign import getContext :: ConnectionConfig -> Effect (Promise InteractionContext)

foreign import _mkWebSocket :: Url -> Effect WebSocket 

foreign import _onWsConnect :: WebSocket -> (Effect Unit) -> Effect Unit

-- we will likely need to change this to json
foreign import _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: WebSocket -> String -> Effect Unit

foreign import _wsClose :: WebSocket -> Effect Unit

foreign import _stringify :: forall a. a -> Effect String

data WebSocket

-- data ConnectionConfig = ConnectionConfig {
  -- host :: String,
  -- port :: Int,
  -- tls :: Boolean,
  -- maxPayload :: Int
-- }

-- data InteractionContext 

-- data Promise a = Promise a 

-- defaultContext :: Effect (Promise InteractionContext)
-- defaultContext = getContext defaultConnConfig 

type Url = String

-- defaultConnConfig :: ConnectionConfig
-- defaultConnConfig = ConnectionConfig  {
  -- host: "locahost",
  -- port: 1337,
  -- tls: false,
  -- maxPayload: 10000
-- }

setupConnectionAndQuery :: Address -> Effect WebSocket
setupConnectionAndQuery addr = do
  ws <- _mkWebSocket "ws://127.0.0.1:1337"
  log "websocket object exists"
  _onWsConnect ws (connectionSuccess ws addr)
  pure ws


type Address = String

type JsonWspRequest a =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: Mirror
  }

type Mirror = { step :: String }

mkJsonWspQuery :: forall a. a -> JsonWspRequest a 
mkJsonWspQuery a = 
  { type : "jsonwsp/request",
    version: "1.0",
    servicename: "ogmios",
    methodname: "Query",
    args: a,
    mirror: { step: "INIT" }
  }

type UtxoQueryParams = { utxo :: Array Address }
type QueryArgs a = { query :: a }
type UtxoQueryBody = JsonWspRequest (QueryArgs UtxoQueryParams)

connectionSuccess :: WebSocket -> Address -> Effect Unit
connectionSuccess ws addr = do
  log "websocket connected successfully"
  let (body :: UtxoQueryBody) = mkJsonWspQuery { query: { utxo: [ addr ] } }
  sBody <- _stringify body
  _onWsMessage ws receiveMsg
  _onWsError ws errorMsg
  log "listener set"
  _wsSend ws sBody
  log "body sent"
  -- _wsClose ws
  -- log "websocket close"
  pure unit

receiveMsg :: String -> Effect Unit
receiveMsg str = do
  log "got msg"
  log str
  pure unit

errorMsg :: String -> Effect Unit
errorMsg str = do
  log "got error:"
  log str
  pure unit
