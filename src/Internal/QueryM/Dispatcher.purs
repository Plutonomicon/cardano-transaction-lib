module Ctl.Internal.QueryM.Dispatcher where

import Prelude

import Aeson (Aeson, JsonDecodeError, stringifyAeson)
import Ctl.Internal.QueryM.JsonWsp (parseJsonWspResponseId)
import Ctl.Internal.QueryM.UniqueId (ListenerId)
import Ctl.Internal.Types.MultiMap (MultiMap)
import Data.Either (Either(Left, Right))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Exception (Error, error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref

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

mkWebsocketDispatch :: Dispatcher -> WebsocketDispatch
mkWebsocketDispatch dispatcher aeson = do
  case parseJsonWspResponseId aeson of
    Left parseError ->
      pure $ Left $ JsonError parseError
    Right reflection -> do
      idMap <- Ref.read dispatcher
      let
        mbAction =
          Map.lookup reflection idMap
            :: Maybe (Aeson -> Effect Unit)
      case mbAction of
        Nothing -> pure $ Left $ ListenerCancelled reflection
        Just action -> pure $ Right $ action aeson

type ShouldResend = Boolean

type Pending = Ref (Map ListenerId RequestBody)
