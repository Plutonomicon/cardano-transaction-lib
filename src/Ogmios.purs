module Ogmios where

-- import Prelude
-- foreign import getContext :: ConnectionConfig -> Effect (Promise InteractionContext)

-- data ConnectionConfig = ConnectionConfig {
  -- host :: String,
  -- port :: Int,
  -- tls :: Boolean
  -- -- maxPayload: number
-- }

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

foreign import _mkWebSocket :: Url -> Effect WebSocket 

foreign import _onWsConnect :: WebSocket -> (Effect Unit) -> Effect Unit

-- we will likely need to change this to json
foreign import _onWsMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _wsSend :: WebSocket -> String -> Effect Unit

data WebSocket

type Url = String


setupConnectionAndQuery :: Effect WebSocket
setupConnectionAndQuery = do
  ws <- _mkWebSocket "ws://127.0.0.1:1337"
  log "websocket object exists"
  _onWsConnect ws (connectionSuccess ws)
  pure ws

connectionSuccess :: WebSocket -> Effect Unit
connectionSuccess ws = do
  log "websocket connected successfully"
