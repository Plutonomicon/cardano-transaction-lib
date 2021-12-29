module Ogmios where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Types.JsonWsp (Address, UtxoQueryBody, mkJsonWspQuery)


foreign import _mkWebSocket :: Url -> Effect WebSocket 

foreign import _onWsConnect :: WebSocket -> (Effect Unit) -> Effect Unit

-- we will likely need to change this to json
foreign import _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: WebSocket -> String -> Effect Unit

foreign import _wsClose :: WebSocket -> Effect Unit

foreign import _stringify :: forall a. a -> Effect String

data WebSocket

type Url = String

setupConnectionAndQuery :: Address -> Effect WebSocket
setupConnectionAndQuery addr = do
  ws <- _mkWebSocket "ws://127.0.0.1:1337"
  log "websocket object exists"
  _onWsConnect ws (connectionSuccess ws addr)
  pure ws

-- this is a collection of functions for 'prototyping'  we can carve library functions off of these as we work toward our mission of building and submitting transactions
-- eventually, our API will be something fairly straightforward like `Websocket -> Config -> Aff Unit` where config will define which wallet we use, etc.
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
