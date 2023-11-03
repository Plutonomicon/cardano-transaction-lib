-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios
module Ctl.Internal.QueryM.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , buildRequest
  , mkCallType
  , JsonRpc2Response
  , decodeResult
  , decodeError
  , ogmiosDecodeErrorToError
  , OgmiosDecodeError(NoResultError, DecodingError, InvalidResponseError)
  , class DecodeOgmios
  , decodeOgmios
  , decodeOgmiosResponse
  , parseJsonRpc2ResponseId
  , decodeAesonJsonRpc2Response
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(AtKey, TypeMismatch, MissingValue)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  , getFieldOptional
  , stringifyAeson
  )
import Ctl.Internal.QueryM.UniqueId (ListenerId, uniqueId)
import Data.Bifunctor (bimap)
import Data.Either (Either(Left), note)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Error, error)
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

decodeAesonJsonRpc2Response
  :: Aeson -> Either OgmiosDecodeError JsonRpc2Response
decodeAesonJsonRpc2Response aeson = bimap InvalidResponseError identity
  $ flip aesonObject aeson
  $ \o -> do
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

-- | A wrapper for tying arguments and response types to request building.
newtype JsonRpc2Call :: Type -> Type -> Type
newtype JsonRpc2Call (i :: Type) (o :: Type) = JsonRpc2Call
  (i -> Effect { body :: Aeson, id :: String })

-- | Creates a "jsonrpc call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => { jsonrpc :: String }
  -> { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkCallType service { method, params } = JsonRpc2Call \i -> do
  req <- mkJsonRpc2Request service { method, params: params i }
  pure { body: encodeAeson req, id: req.id }

-- | Create a JsonRpc2 request body and id
buildRequest
  :: forall (i :: Type) (o :: Type)
   . JsonRpc2Call i o
  -> i
  -> Effect { body :: Aeson, id :: String }
buildRequest (JsonRpc2Call c) = c

data OgmiosDecodeError
  -- Server didn't respond with "result", responded with given "error".
  = NoResultError (Maybe Aeson)
  -- Parsing of result failed
  | DecodingError JsonDecodeError
  -- Parsing of JsonRpc2Response failed
  | InvalidResponseError JsonDecodeError

instance Show OgmiosDecodeError where
  show (DecodingError err) = show err
  show (NoResultError err) =
    "Server didn't respond with result. Responded with error: " <> maybe
      ""
      stringifyAeson
      err
  -- Parsing of JsonRpc2Response failed
  show (InvalidResponseError err) =     "Couldn't parse the response: " <> show err

ogmiosDecodeErrorToError :: OgmiosDecodeError -> Error
ogmiosDecodeErrorToError err = error $ show err

-- | Variation of DecodeAeson for ogmios response, defines how to parse full ogmios reponse.
-- We usually parse just the content of the "result" field, 
-- but sometimes also "error" field, hence a class other than DecodeAeson.
class DecodeOgmios o where
  decodeOgmios :: JsonRpc2Response -> Either OgmiosDecodeError o

decodeOgmiosResponse :: forall o . DecodeOgmios o =>  Aeson -> Either OgmiosDecodeError o
decodeOgmiosResponse = decodeOgmios <=< decodeAesonJsonRpc2Response

-- | Decode "result" field of ogmios response with DecodeAeson.
decodeResult
  :: forall o. DecodeAeson o => JsonRpc2Response -> Either OgmiosDecodeError o
decodeResult response = do
  result <- note (NoResultError response.error) $ response.result
  bimap DecodingError identity $ decodeAeson result

-- | Decode "error" field of ogmios response with DecodeAeson.
decodeError
  :: forall o. DecodeAeson o => JsonRpc2Response -> Either OgmiosDecodeError o
decodeError response = bimap DecodingError identity $ do
  error <- note (AtKey "error" MissingValue) response.error
  decodeAeson error

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
