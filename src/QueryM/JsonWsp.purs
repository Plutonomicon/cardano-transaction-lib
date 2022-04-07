-- | Provides basics types and operations for working with JSON RPC protocol
-- | used by Ogmios and ogmios-datum-cache
module QueryM.JsonWsp
  ( JsonWspRequest
  , JsonWspResponse
  , JsonWspCall
  , Mirror
  , mkCallType
  , buildRequest
  , parseJsonWspResponse
  , parseFieldToString
  , parseFieldToUInt
  , parseFieldToBigInt
  ) where

import Prelude

import Aeson (class DecodeAeson, Aeson,
 caseAesonBigInt, caseAesonObject, caseAesonString, caseAesonUInt, decodeAeson, getField)
import Data.Argonaut (class EncodeJson,
 Json, JsonDecodeError(TypeMismatch), encodeJson)
import Data.BigInt as BigInt
import Data.Either (Either(Left, Right))
import Data.UInt as UInt
import Effect (Effect)
import Foreign.Object (Object)
import Record as Record
import Type.Proxy (Proxy)


-- creates a unique id prefixed by its argument
foreign import _uniqueId :: String -> Effect String


-- | Structure of all json wsp websocket requests
-- described in: https://ogmios.dev/getting-started/basics/
type JsonWspRequest a =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , args :: a
  , mirror :: Mirror
  }


-- | Convenience helper function for creating `JsonWspRequest a` objects
mkJsonWspRequest
  :: forall a
   . { type :: String
     , version :: String
     , servicename :: String }
  -> { methodname :: String
     , args :: a }
  -> Effect (JsonWspRequest a)
mkJsonWspRequest service method = do
  id <- _uniqueId $ method.methodname <> "-"
  pure $
    Record.merge { mirror: {step: "INIT", id}} $
    Record.merge service method


-- | Structure of all json wsp websocket responses
-- described in: https://ogmios.dev/getting-started/basics/
type JsonWspResponse a =
  { type :: String
  , version :: String
  , servicename :: String
  , methodname :: String
  , result :: a
  , reflection :: Mirror
  }

-- this is fully determined by us - we can adjust this type as we have more complex
-- needs, it always just gets echoed back, so it is useful for req/res pairing
-- | A type we use to reflect jsonwsp request ids.
type Mirror = { step :: String, id :: String }


-- | A wrapper for tying arguments and response types to request building.
newtype JsonWspCall :: Type -> Type -> Type
newtype JsonWspCall i o = JsonWspCall (i -> Effect { body :: Json, id :: String})

-- | Creates a "jsonwsp call" which ties together request input and response output types
-- | along with a way to create a request object.
mkCallType
  :: forall a i o
   . EncodeJson (JsonWspRequest a)
  => { type :: String
     , version :: String
     , servicename :: String }
  -> { methodname :: String, args :: i -> a }
  -> Proxy o
  -> JsonWspCall i o
mkCallType service {methodname, args} _ = JsonWspCall $ \i -> do
  req <- mkJsonWspRequest service {methodname, args: args i}
  pure { body: encodeJson req, id: req.mirror.id }


-- | Create a JsonWsp request body and id
buildRequest :: forall i o. JsonWspCall i o -> i -> Effect { body :: Json, id :: String}
buildRequest (JsonWspCall c) = c


-- | Polymorphic response parser
parseJsonWspResponse
  :: forall a
   . DecodeAeson a
  => Aeson
  -> Either JsonDecodeError (JsonWspResponse a)
parseJsonWspResponse = aesonObject
  ( \o -> do
      typeField <- parseFieldToString o "type"
      version <- parseFieldToString o "version"
      servicename <- parseFieldToString o "servicename"
      methodname <- parseFieldToString o "methodname"
      result <- decodeAeson =<< getField o "result"
      reflection <- parseMirror =<< getField o "reflection"
      pure { "type": typeField, version, servicename, methodname, result, reflection }
  )

-- | Helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
  . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "expected object"))

-- parsing json

-- | Parses json string at a given field to an ordinary string
parseFieldToString :: Object Aeson -> String -> Either JsonDecodeError String
parseFieldToString o str =
  caseAesonString (Left (TypeMismatch ("expected field: '" <> str <> "' as a String"))) Right =<< getField o str

-- | Parses a string at the given field to a UInt
parseFieldToUInt :: Object Aeson -> String -> Either JsonDecodeError UInt.UInt
parseFieldToUInt o str = do
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a UInt"
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "index" is a string in our local example.
  caseAesonUInt (Left err) Right =<< getField o str

-- -- The below doesn't seem to work with Ogmios query test (AffInterface)
-- -- eventhough it seems more reasonable.
-- num <- decodeNumber =<< getField o str
-- note err $ UInt.fromNumber' num
-- | Parses a string at the given field to a BigInt
parseFieldToBigInt :: Object Aeson -> String -> Either JsonDecodeError BigInt.BigInt
parseFieldToBigInt o str = do
  -- We use string parsing for Ogmios (AffInterface tests) but also change Medea
  -- schema and UtxoQueryResponse.json to be a string to pass (local) parsing
  -- tests. Notice "coins" is a string in our local example.
  let err = TypeMismatch $ "expected field: '" <> str <> "' as a BigInt"
  caseAesonBigInt (Left err) Right =<< getField o str

-- | A parser for the `Mirror` type.
parseMirror :: Aeson -> Either JsonDecodeError Mirror
parseMirror = caseAesonObject (Left (TypeMismatch "expected object")) $
  ( \o -> do
      step <- parseFieldToString o "step"
      id <- parseFieldToString o "id"
      pure { step, id }
  )
