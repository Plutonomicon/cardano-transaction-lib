module Ctl.Internal.QueryM.Dispatcher where

import Prelude

import Aeson (Aeson, JsonDecodeError, stringifyAeson)
import Ctl.Internal.QueryM.UniqueId (ListenerId)
import Ctl.Internal.Types.MultiMap (MultiMap)
import Data.Either (Either)
import Data.Map (Map)
import Effect (Effect)
import Effect.Exception (Error, error, message)
import Effect.Ref (Ref)

data DispatchError
  = JsError Error
  | JsonError JsonDecodeError
  -- Server response has been parsed succesfully, but it contains error
  -- message
  | FaultError Aeson
  -- The listener that was added for this message has been cancelled
  | ListenerCancelled ListenerId

instance Show DispatchError where
  show (JsError err) = "(JsError (message " <> show (message err) <> "))"
  show (JsonError jsonErr) = "(JsonError " <> show jsonErr <> ")"
  show (FaultError aeson) = "(FaultError " <> show aeson <> ")"
  show (ListenerCancelled listenerId) =
    "(ListenerCancelled " <> show listenerId <> ")"

dispatchErrorToError :: DispatchError -> Error
dispatchErrorToError (JsError err) = err
dispatchErrorToError (JsonError err) = error $ show err
dispatchErrorToError (FaultError err) =
  error $ "Server responded with `fault`: " <> stringifyAeson err
dispatchErrorToError (ListenerCancelled listenerId) =
  error $ "Listener cancelled (" <> listenerId <> ")"

type WebsocketDispatch =
  Aeson -> Effect (Either DispatchError (Effect Unit))

type DispatchIdMap response = Ref
  (MultiMap ListenerId (Either DispatchError response -> Effect Unit))

type RequestBody = String

type Dispatcher = Ref (Map ListenerId (Aeson -> Effect Unit))

type ShouldResend = Boolean

type Pending = Ref (Map ListenerId RequestBody)
