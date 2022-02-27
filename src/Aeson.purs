-- | Argonaut can't decode long integers the way Aeson encodes them: they
-- | lose precision on the stage of `JSON.parse` call, which we can't really
-- | control. This module is a hacky solution allowing us to preserve long
-- | integers while decoding.
-- | The idea is that we process JSON-as-string in FFI, exctracting all numbers
-- | into a separate array named "index", where they are represented as strings,
-- | and place that index alongside the original json. We modify the original
-- | JSON such that it contains indices of original numbers in the array,
-- | instead of the actual numbers.
-- |
-- | E.g. from `{ "a": 42, "b": 24 }` we get `{ json: {"a": 0, "b": 1 }, index: [ 42, 24 ] }`.
-- |
-- | Then, in decoders for `Int` and `BigInt` we access that array to get the
-- | values back.
-- |
-- | Known limitations: does not support Record decoding (no GDecodeJson-like
-- | machinery). But it is possible to decode records manually, because
-- | `getField` is implemented.
-- |
-- | Does not support optional fields because they're not needeed yet, but this
-- |  functionality can be adapted from argonaut similarly to `getField`.
module Aeson
  ( NumberIndex
  , class DecodeAeson
  , Aeson(..)
  , decodeAesonViaJson
  , decodeAeson
  , decodeAesonString
  , getField
  , (.:)
  , getJson
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Traversable (class Traversable, for)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as FO

-- | A piece of JSON where all numbers are extracted away into `NumberIndex`.
newtype Aeson = Aeson { json :: Json, index :: NumberIndex }

getJson :: Aeson -> Json
getJson (Aeson { json }) = json

-- | A list of numbers extracted from Json, as they appear in the payload.
type NumberIndex = Array String

class DecodeAeson a where
  decodeAeson :: Aeson -> Either JsonDecodeError a

instance DecodeAeson Int where
  decodeAeson aeson@(Aeson { index }) = do
    -- Numbers are replaced by their index in the array.
    ix <- decodeAesonViaJson aeson
    numberStr <- note MissingValue (index Array.!! ix)
    note MissingValue $ Int.fromString numberStr

instance DecodeAeson BigInt where
  decodeAeson aeson@(Aeson { index }) = do
    -- Numbers are replaced by their index in the array.
    ix <- decodeAesonViaJson aeson
    numberStr <- note MissingValue (index Array.!! ix)
    note MissingValue $ BigInt.fromString numberStr

instance DecodeAeson String where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Number where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Json where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Aeson where
  decodeAeson = pure

instance (Traversable t, DecodeAeson a, DecodeJson (t Json)) => DecodeAeson (t a) where
  decodeAeson (Aeson { index, json }) = do
    jsons :: t _ <- decodeJson json
    for jsons (\valueJson -> decodeAeson (Aeson { json: valueJson, index }))

getField :: forall a. DecodeAeson a => FO.Object Aeson -> String -> Either JsonDecodeError a
getField aesonObject field = getField' decodeAeson aesonObject field

-- | Adapted from `Data.Argonaut.Decode.Decoders`
getField'
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> FO.Object Aeson
  -> String
  -> Either JsonDecodeError a
getField' decoder obj str =
  maybe
    (Left $ AtKey str MissingValue)
    (lmap (AtKey str) <<< decoder)
    (FO.lookup str obj)

infix 7 getField as .:

-- TODO: add getFieldOptional if ever needed.

foreign import parseJsonExtractingIntegers
  :: (forall a b. a -> b -> Tuple a b) -> String -> String /\ NumberIndex

-- | Ignore numeric index and reuse Argonaut decoder.
decodeAesonViaJson :: forall a. DecodeJson a => Aeson -> Either JsonDecodeError a
decodeAesonViaJson (Aeson { json }) = decodeJson json

decodeAesonString :: forall a. DecodeAeson a => String -> Either JsonDecodeError a
decodeAesonString payload = do
  json <- note MissingValue $ hush $ jsonParser patchedPayload
  decodeAeson (Aeson { index, json })
  where
  patchedPayload /\ index = parseJsonExtractingIntegers Tuple payload
