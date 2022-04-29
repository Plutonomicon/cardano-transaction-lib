module JsWebSocket
  ( JsWebSocket
  , Url
  , mkWebSocket
  , onWsConnect
  , onWsError
  , onWsMessage
  , wsSend
  , wsClose
  , wsWatch
  ) where

import Prelude

import Effect (Effect)

--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import data JsWebSocket :: Type

type Url = String

foreign import mkWebSocket
  :: (String -> Effect Unit)
  -> Url
  -> Effect JsWebSocket

foreign import onWsConnect :: JsWebSocket -> (Effect Unit) -> Effect Unit

foreign import onWsMessage
  :: JsWebSocket
  -> (String -> Effect Unit) -- logger
  -> (String -> Effect Unit) -- handler
  -> Effect Unit

foreign import onWsError
  :: JsWebSocket
  -> (String -> Effect Unit) -- logger
  -> (String -> Effect Unit) -- handler
  -> Effect Unit

foreign import wsSend
  :: JsWebSocket -> (String -> Effect Unit) -> String -> Effect Unit

foreign import wsClose :: JsWebSocket -> Effect Unit

foreign import wsWatch
  :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit -> Effect Unit
