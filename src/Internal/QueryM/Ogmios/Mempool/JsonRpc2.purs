-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios
module Ctl.Internal.QueryM.Ogmios.Mempool.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , buildRequest
  , mkCallType
  , parseJsonRpc2ResponseId
  ) where

import Prelude

import Aeson
  ( class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Data.Either (Either(Left))
import Effect (Effect)
import Record as Record

-- | Creates a unique id prefixed by its argument
foreign import uniqueId :: String -> Effect String

-- | Structure of all json rpc2.0 websocket requests
-- described in:  https://ogmios.dev/getting-started/basics/
type JsonRpc2Request (a :: Type) =
  { jsonrpc :: String
  , method :: String
  , params :: a
  , id :: String
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

-- | Parse just ID from the response
parseJsonRpc2ResponseId
  :: Aeson
  -> Either JsonDecodeError String
parseJsonRpc2ResponseId =
  caseAesonObject (Left (TypeMismatch "Object")) $ flip getField "id"

