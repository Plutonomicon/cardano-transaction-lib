module Ctl.Internal.QueryM.Ogmios.Mempool.JsWebSocket
  ( JsWebSocket
  , ListenerRef
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _removeOnWsError
  , _onWsMessage
  , _wsSend
  , _wsReconnect
  , _wsClose
  , _wsFinalize
  ) where

import Prelude

import Effect (Effect)

--------------------------------------------------------------------------------
-- Websocket Basics
--------------------------------------------------------------------------------
foreign import data JsWebSocket :: Type

-- | Opaque listener reference that allows to cancel a listener
foreign import data ListenerRef :: Type

type Url = String

foreign import _mkWebSocket
  :: (String -> Effect Unit)
  -> Url
  -> Effect JsWebSocket

foreign import _onWsConnect
  :: JsWebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage
  :: JsWebSocket
  -> (String -> Effect Unit) -- logger
  -> (String -> Effect Unit) -- handler
  -> Effect Unit

foreign import _onWsError
  :: JsWebSocket
  -> (String -> Effect Unit) -- handler
  -> Effect ListenerRef

-- | Call `removeEventListener` for a given listener.
foreign import _removeOnWsError
  :: JsWebSocket
  -> ListenerRef
  -> Effect Unit

foreign import _wsSend
  :: JsWebSocket -> (String -> Effect Unit) -> String -> Effect Unit

foreign import _wsReconnect
  :: JsWebSocket -> Effect Unit

foreign import _wsClose :: JsWebSocket -> Effect Unit

foreign import _wsFinalize :: JsWebSocket -> Effect Unit
