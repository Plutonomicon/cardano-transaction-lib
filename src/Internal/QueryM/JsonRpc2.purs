-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios
module Ctl.Internal.QueryM.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , buildRequest
  , mkCallType
  , parseJsonRpc2Response
  , ParseResponse(ParseResponse)
  , JsonRpc2Response
  , successResponse
  , anyResponse
  , parseJsonRpc2ResponseId
  )
  where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, Aeson, JsonDecodeError(..), caseAesonObject, decodeAeson, encodeAeson, getField, getFieldOptional)
import Ctl.Internal.QueryM.UniqueId (ListenerId, uniqueId)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Foreign.Object (Object)
import Record as Record

-- | Structure of all json rpc2.0 websocket requests
-- described in:  https://ogmios.dev/getting-started/basics/
type JsonRpc2Request (a :: Type) =
  { jsonrpc :: String
  , method :: String
  , params :: a
  , id :: ListenerId
  }

-- | Convenience helper function for creating `JsonRpc2Request a` objects
mkJsonRpc2Request
  :: forall (a :: Type)
   . { jsonrpc :: String }
  -> { method :: String
     , params :: a
     }
  -> Effect (JsonRpc2Request a)
mkJsonRpc2Request service method = do
  id <- uniqueId $ method.method <> "-"
  pure
    $ Record.merge { id }
    $ Record.merge service method

-- | Structure of all json rpc websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonRpc2Response =
  { jsonrpc :: String
  -- methodname is not always present if `error` is not empty
  , method :: Maybe String
  , result :: Maybe Aeson
  , error :: Maybe Aeson
  , id :: ListenerId
  }

data ParseResponse (o :: Type) = ParseResponse (JsonRpc2Response -> Either JsonDecodeError o)

successResponse :: forall o. DecodeAeson o => ParseResponse o
successResponse = ParseResponse $ \res -> do 
  result <- fromMaybe (Left (AtKey "result" MissingValue)) (pure <$> res.result)
  decodeAeson result

anyResponse :: forall a. (Aeson -> Either JsonDecodeError a) -> (Aeson -> Either JsonDecodeError a) -> ParseResponse a
anyResponse decodeError decodeResult = ParseResponse $ \{result, error} -> parseError error <|> parseResult result
  where 
  
  parseResult result = do
    res <- fromMaybe (Left $ AtKey "result" MissingValue) $ pure <$> result
    decodeResult res

  parseError error = do
    err <- fromMaybe (Left $ AtKey "error" MissingValue) (pure <$> error)
    decodeError err

-- | A wrapper for tying arguments and response types to request building.
data JsonRpc2Call :: Type -> Type -> Type
data JsonRpc2Call (i :: Type) (o :: Type) = JsonRpc2Call
  (ParseResponse o) (i -> Effect { body :: Aeson, id :: String })

-- | Creates a "jsonrpc call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => { jsonrpc :: String }
  -> ParseResponse o
  -> { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkCallType service response { method, params }  = JsonRpc2Call response $ \i -> do
  req <- mkJsonRpc2Request service { method, params: params i }
  pure { body: encodeAeson req, id: req.id }

-- | Create a JsonRpc2 request body and id
buildRequest
  :: forall (i :: Type) (o :: Type)
   . JsonRpc2Call i o
  -> i
  -> Effect { body :: Aeson, id :: String }
buildRequest (JsonRpc2Call _ c) = c

-- | Polymorphic response parser
parseJsonRpc2Response :: forall (i :: Type) (o :: Type)
   . JsonRpc2Call i o -> Aeson -> Either JsonDecodeError o
parseJsonRpc2Response (JsonRpc2Call (ParseResponse parse) _) aeson = do
  res <- flip aesonObject aeson $ \o -> do
      jsonrpc <- getField o "jsonrpc"
      method <- getFieldOptional o "method"
      result <- getFieldOptional o "result"
      error <- getFieldOptional o "error"
      id <- getField o "id"
      pure
        { jsonrpc
        , method
        , result
        , error
        , id
        }
  parse res

-- | Parse just ID from the response
parseJsonRpc2ResponseId
  :: Aeson
  -> Either JsonDecodeError ListenerId
parseJsonRpc2ResponseId =
  aesonObject $ flip getField "id"

-- | Helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "expected object"))
