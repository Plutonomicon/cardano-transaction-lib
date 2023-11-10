-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios
module Ctl.Internal.QueryM.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , buildRequest
  , mkCallType
  , JsonRpc2Response
  , decodeResult
  , ogmiosDecodeErrorToError
  , OgmiosDecodeError(ResultDecodingError, InvalidResponse, ErrorResponse)
  , OgmiosError(OgmiosError)
  , class DecodeOgmios
  , decodeOgmios
  , decodeErrorOrResult
  , parseJsonRpc2ResponseId
  , decodeAesonJsonRpc2Response
  , pprintOgmiosDecodeError
  , pprintOgmiosError
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  , getFieldOptional
  , printJsonDecodeError
  , stringifyAeson
  )
import Ctl.Internal.QueryM.UniqueId (ListenerId, uniqueId)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.These (These(That, Both), theseLeft, theseRight)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
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
  :: Aeson -> Either JsonDecodeError JsonRpc2Response
decodeAesonJsonRpc2Response = aesonObject $ \o -> do
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

newtype OgmiosError = OgmiosError
  { code :: Int, message :: String, data :: Maybe Aeson }

derive instance Generic OgmiosError _
derive instance Newtype OgmiosError _

instance Show OgmiosError where
  show = genericShow

pprintOgmiosError :: OgmiosError -> String
pprintOgmiosError (OgmiosError err) = stringifyAeson $ encodeAeson err

instance DecodeAeson OgmiosError where
  decodeAeson = aesonObject \o -> do
    code <- getField o "code"
    message <- getField o "message"
    dat <- getFieldOptional o "data"
    pure $ OgmiosError { code, message, data: dat }

data OgmiosDecodeError
  -- Server responded with error.
  = ErrorResponse (Maybe OgmiosError)
  -- Server responded with result, parsing of which failed
  | ResultDecodingError JsonDecodeError
  -- Received JsonRpc2Response was not of the right format.
  | InvalidResponse JsonDecodeError

derive instance Generic OgmiosDecodeError _

instance Show OgmiosDecodeError where
  show = genericShow

pprintOgmiosDecodeError :: OgmiosDecodeError -> String
pprintOgmiosDecodeError (ErrorResponse err) = "Ogmios responded with error: " <>
  maybe "<Actually no response>" pprintOgmiosError err
pprintOgmiosDecodeError (ResultDecodingError err) =
  "Failed to parse the result: " <> printJsonDecodeError err
pprintOgmiosDecodeError (InvalidResponse err) =
  "Ogmios response was not of the right format: " <> printJsonDecodeError err

ogmiosDecodeErrorToError :: OgmiosDecodeError -> Error
ogmiosDecodeErrorToError err = error $ pprintOgmiosDecodeError err

-- | Variation of DecodeAeson for ogmios response, defines how to parse full ogmios reponse.
-- We usually parse just the content of the "result" field,
-- but sometimes also "error" field, hence a class other than DecodeAeson.
class DecodeOgmios o where
  decodeOgmios :: Aeson -> Either OgmiosDecodeError o

-- | Given how to parse result or error fields,
-- defines a parser of the full json2rpc response.
makeDecodeOgmios
  :: forall o
   . These
       { parseError :: Aeson -> Either JsonDecodeError o }
       { parseResult :: Aeson -> Either JsonDecodeError o }
  -> Aeson
  -> Either OgmiosDecodeError o
makeDecodeOgmios decoders aeson = do
  json <- lmap InvalidResponse $ decodeAesonJsonRpc2Response aeson
  let merr = _.parseError <$> theseLeft decoders <*> json.error
  let mres = _.parseResult <$> theseRight decoders <*> json.result
  case (mres /\ merr) of
    -- Expected result, got it
    Just (Right x) /\ _ -> pure x
    -- Expected result, got it in a wrong format
    Just (Left err) /\ _ -> Left $ ResultDecodingError err
    -- Got an expected error
    _ /\ Just (Right x) -> pure x
    -- Got an unexpected error
    _ -> do
      err :: Maybe OgmiosError <- sequence $
        lmap InvalidResponse <<< decodeAeson <$> json.error
      Left $ ErrorResponse err

-- | Decode "result" field of ogmios response.
decodeResult
  :: forall o
   . (Aeson -> Either JsonDecodeError o)
  -> Aeson
  -> Either OgmiosDecodeError o
decodeResult decodeAeson = makeDecodeOgmios $ That { parseResult: decodeAeson }

-- | Decode "result" field or if absent the error field of ogmios response.
decodeErrorOrResult
  :: forall o
   . { parseError :: (Aeson -> Either JsonDecodeError o) }
  -> { parseResult :: (Aeson -> Either JsonDecodeError o) }
  -> Aeson
  -> Either OgmiosDecodeError o
decodeErrorOrResult err res = makeDecodeOgmios $ Both err res

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
