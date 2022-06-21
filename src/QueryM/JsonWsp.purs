-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios and ogmios-datum-cache
module QueryM.JsonWsp
  ( JsonWspRequest
  , JsonWspResponse
  , JsonWspCall
  , mkCallType
  , buildRequest
  , parseJsonWspResponse
  , parseJsonWspResponseId
  , parseFieldToUInt
  , parseFieldToBigInt
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonBigInt
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , decodeAeson
  , encodeAeson
  , getField
  , getFieldOptional
  )
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.UInt as UInt
import Effect (Effect)
import Foreign.Object (Object)
import QueryM.UniqueId (ListenerId, uniqueId)
import Record as Record
import Type.Proxy (Proxy)

-- | Structure of all json wsp websocket requests
-- described in: https://ogmios.dev/getting-started/basics/
type JsonWspRequest (a :: Type) =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: ListenerId
  }

-- | Convenience helper function for creating `JsonWspRequest a` objects
mkJsonWspRequest
  :: forall (a :: Type)
   . { type :: String
     , version :: String
     , servicename :: String
     }
  -> { methodname :: String
     , args :: a
     }
  -> Effect (JsonWspRequest a)
mkJsonWspRequest service method = do
  id <- uniqueId $ method.methodname <> "-"
  pure
    $ Record.merge { mirror: id }
    $
      Record.merge service method

-- | Structure of all json wsp websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonWspResponse (a :: Type) =
  { type :: String
  , version :: String
  , servicename :: String
  -- methodname is not always present if `fault` is not empty
  , methodname :: Maybe String
  , result :: Maybe a
  , fault :: Maybe Aeson
  , reflection :: ListenerId
  }

-- | A wrapper for tying arguments and response types to request building.
newtype JsonWspCall :: Type -> Type -> Type
newtype JsonWspCall (i :: Type) (o :: Type) = JsonWspCall
  (i -> Effect { body :: Aeson, id :: String })

-- | Creates a "jsonwsp call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonWspRequest a)
  => { type :: String
     , version :: String
     , servicename :: String
     }
  -> { methodname :: String, args :: i -> a }
  -> Proxy o
  -> JsonWspCall i o
mkCallType service { methodname, args } _ = JsonWspCall $ \i -> do
  req <- mkJsonWspRequest service { methodname, args: args i }
  pure { body: encodeAeson req, id: req.mirror }

-- | Create a JsonWsp request body and id
buildRequest
  :: forall (i :: Type) (o :: Type)
   . JsonWspCall i o
  -> i
  -> Effect { body :: Aeson, id :: String }
buildRequest (JsonWspCall c) = c

-- | Polymorphic response parser
parseJsonWspResponse
  :: forall (a :: Type)
   . DecodeAeson a
  => Aeson
  -> Either JsonDecodeError (JsonWspResponse a)
parseJsonWspResponse = aesonObject $ \o -> do
  typeField <- getField o "type"
  version <- getField o "version"
  servicename <- getField o "servicename"
  methodname <- getFieldOptional o "methodname"
  result <- traverse decodeAeson =<< getFieldOptional o "result"
  fault <- traverse decodeAeson =<< getFieldOptional o "fault"
  reflection <- parseMirror =<< getField o "reflection"
  pure
    { "type": typeField
    , version
    , servicename
    , methodname
    , result
    , fault
    , reflection
    }

-- | Parse just ID from the response
parseJsonWspResponseId
  :: Aeson
  -> Either JsonDecodeError ListenerId
parseJsonWspResponseId = aesonObject $ \o -> do
  parseMirror =<< getField o "reflection"

-- | Helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "expected object"))

-- parsing json

-- | Parses a string at the given field to a UInt
parseFieldToUInt :: Object Aeson -> String -> Either JsonDecodeError UInt.UInt
parseFieldToUInt o str = do
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "index" is a string in our local example.
  caseAesonUInt (Left err) Right =<< getField o str
  where
  err :: JsonDecodeError
  err = TypeMismatch $ "expected field: '" <> str <> "' as a UInt"

-- -- The below doesn't seem to work with Ogmios query test (AffInterface)
-- -- eventhough it seems more reasonable.
-- num <- decodeNumber =<< getField o str
-- note err $ UInt.fromNumber' num
-- | Parses a string at the given field to a BigInt
parseFieldToBigInt
  :: Object Aeson -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToBigInt o str = do
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "coins" is a string in our local example.
  caseAesonBigInt (Left err) Right =<< getField o str
  where
  err :: JsonDecodeError
  err = TypeMismatch $ "expected field: '" <> str <> "' as a BigInt"

-- | A parser for the `Mirror` type.
parseMirror :: Aeson -> Either JsonDecodeError ListenerId
parseMirror = caseAesonString (Left (TypeMismatch "expected string")) pure
