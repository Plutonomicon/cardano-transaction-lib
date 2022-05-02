module JsWebSocket
  ( JsWebSocket
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsSend
  , _wsClose
  , _wsWatch
  ) where

import Prelude

import Effect (Effect)

--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import data JsWebSocket :: Type

type Url = String

foreign import _mkWebSocket
  :: (String -> Effect Unit)
  -> Url
  -> Effect JsWebSocket

foreign import _onWsConnect :: JsWebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage
  :: JsWebSocket
  -> (String -> Effect Unit) -- logger
  -> (String -> Effect Unit) -- handler
  -> Effect Unit

foreign import _onWsError
  :: JsWebSocket
  -> (String -> Effect Unit) -- logger
  -> (String -> Effect Unit) -- handler
  -> Effect Unit

foreign import _wsSend
  :: JsWebSocket -> (String -> Effect Unit) -> String -> Effect Unit

foreign import _wsClose :: JsWebSocket -> Effect Unit

foreign import _wsWatch
  :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit -> Effect Unit
