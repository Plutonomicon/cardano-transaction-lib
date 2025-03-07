module Ctl.Internal.QueryM.Ogmios.Mempool.Dispatcher
  ( DispatchError(JsonError, FaultError, ListenerCancelled)
  , Dispatcher
  , RequestBody
  , WebsocketDispatch
  , dispatchErrorToError
  , mkWebsocketDispatch
  , newDispatcher
  , ListenerId
  ) where

import Prelude

import Aeson (Aeson, JsonDecodeError, stringifyAeson)
import Ctl.Internal.QueryM.Ogmios.Mempool.JsonRpc2 (parseJsonRpc2ResponseId)
import Data.Either (Either(Left, Right))
import Data.Map (Map)
import Data.Map (empty, lookup) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref (new, read) as Ref

type ListenerId = String

data DispatchError
  = JsonError JsonDecodeError
  -- Server response has been parsed succesfully, but it contains error
  -- message
  | FaultError Aeson
  -- The listener that was added for this message has been cancelled
  | ListenerCancelled ListenerId

instance Show DispatchError where
  show (JsonError jsonErr) = "(JsonError " <> show jsonErr <> ")"
  show (FaultError aeson) = "(FaultError " <> show aeson <> ")"
  show (ListenerCancelled listenerId) =
    "(ListenerCancelled " <> show listenerId <> ")"

dispatchErrorToError :: DispatchError -> Error
dispatchErrorToError (JsonError err) = error $ show err
dispatchErrorToError (FaultError err) =
  error $ "Server responded with `fault`: " <> stringifyAeson err
dispatchErrorToError (ListenerCancelled listenerId) =
  error $ "Listener cancelled (" <> listenerId <> ")"

type WebsocketDispatch =
  Aeson -> Effect (Either DispatchError (Effect Unit))

type RequestBody = String

type Dispatcher = Ref (Map ListenerId (Aeson -> Effect Unit))

newDispatcher :: Effect Dispatcher
newDispatcher = Ref.new Map.empty

mkWebsocketDispatch :: Dispatcher -> WebsocketDispatch
mkWebsocketDispatch dispatcher aeson = do
  case parseJsonRpc2ResponseId aeson of
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

