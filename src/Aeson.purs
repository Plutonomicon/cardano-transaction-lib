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
-- | E.g. from `{ "a": 42, "b": 24 }` we get
-- | `{ json: {"a": 0, "b": 1 }, index: [ "42", "24" ] }`.
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
  , Aeson
  , (.:)
  , caseAeson
  , decodeAeson
  , decodeAesonString
  , getField
  , getFieldNested
  , jsonToAeson
  , parseJsonStringToAeson
  , toStringifiedNumbersJson
  ) where

import Prelude

import Control.Lazy (fix)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), caseJson, caseJsonObject, decodeJson, fromArray, fromObject, jsonNull, stringify)
import Data.Argonaut.Encode.Encoders (encodeBoolean, encodeString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldM)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), fromRight, hush, note)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, for)
import Data.Typelevel.Undefined (undefined)
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)

-- | A piece of JSON where all numbers are replaced with their indexes
newtype AesonPatchedJson = AesonPatchedJson Json

-- | A piece of JSON where all numbers are extracted into `NumberIndex`.
newtype Aeson = Aeson { patchedJson :: AesonPatchedJson, numberIndex :: NumberIndex }

-- | A list of numbers extracted from Json, as they appear in the payload.
type NumberIndex = Array String

class DecodeAeson (a :: Type) where
  decodeAeson :: Aeson -> Either JsonDecodeError a

instance DecodeAeson Int where
  decodeAeson aeson@(Aeson { numberIndex }) = do
    -- Numbers are replaced by their index in the array.
    ix <- decodeAesonViaJson aeson
    numberStr <- note MissingValue (numberIndex Array.!! ix)
    note MissingValue $ Int.fromString numberStr

instance DecodeAeson BigInt where
  decodeAeson aeson@(Aeson { numberIndex }) = do
    -- Numbers are replaced by their index in the array.
    ix <- decodeAesonViaJson aeson
    numberStr <- note MissingValue (numberIndex Array.!! ix)
    note MissingValue $ BigInt.fromString numberStr

instance DecodeAeson Boolean where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson String where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Number where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Json where
  decodeAeson = Right <<< toStringifiedNumbersJson

instance DecodeAeson Aeson where
  decodeAeson = pure

instance (Traversable t, DecodeAeson a, DecodeJson (t Json)) => DecodeAeson (t a) where
  decodeAeson (Aeson { numberIndex, patchedJson: AesonPatchedJson pJson }) = do
    jsons :: t _ <- map AesonPatchedJson <$> decodeJson pJson
    for jsons (\patchedJson -> decodeAeson (Aeson { patchedJson, numberIndex }))

caseAeson
  :: forall a
   . { caseNull :: Unit -> a
     , caseBoolean :: Boolean -> a
     , caseNumber :: String -> a
     , caseString :: String -> a
     , caseArray :: Array Aeson -> a
     , caseObject :: Object Aeson -> a
     }
  -> Aeson
  -> a
caseAeson
  { caseNull, caseBoolean, caseNumber, caseString, caseArray, caseObject }
  (Aeson { numberIndex, patchedJson: AesonPatchedJson pJson }) = caseJson
  caseNull
  caseBoolean
  (coerceNumber >>> unsafeIndex numberIndex >>> caseNumber)
  caseString
  (map mkAeson >>> caseArray)
  (map mkAeson >>> caseObject)
  pJson
  where
  mkAeson :: Json -> Aeson
  mkAeson json = Aeson { patchedJson: AesonPatchedJson json, numberIndex }

  -- will never get index out of bounds
  unsafeIndex :: forall (x :: Type). Array x -> Int -> x
  unsafeIndex arr ix = unsafePartial $ Array.unsafeIndex arr ix

  -- will never encounter non int number
  coerceNumber :: Number -> Int
  coerceNumber = round

-- | Replaces indexes in the Aeson's payload with stringified
--   numbers from numberIndex.
--   Given original payload of: `{"a": 10}`
--   The result will be an Json object representing: `{"a": "10"}`
toStringifiedNumbersJson :: Aeson -> Json
toStringifiedNumbersJson = fix \_ ->
  caseAeson
    { caseNull: const jsonNull
    , caseBoolean: encodeBoolean
    , caseNumber: encodeString
    , caseString: encodeString
    , caseArray: map toStringifiedNumbersJson >>> fromArray
    , caseObject: map toStringifiedNumbersJson >>> fromObject
    }

getField
  :: forall (a :: Type)
   . DecodeAeson a
  => FO.Object Aeson
  -> String
  -> Either JsonDecodeError a
getField aesonObject field = getField' decodeAeson aesonObject field

-- | Adapted from `Data.Argonaut.Decode.Decoders`
getField'
  :: forall (a :: Type)
   . (Aeson -> Either JsonDecodeError a)
  -> FO.Object Aeson
  -> String
  -> Either JsonDecodeError a
getField' decoder obj str =
  maybe
    (Left $ AtKey str MissingValue)
    (lmap (AtKey str) <<< decoder)
    (FO.lookup str obj)

-- | Returns an Aeson available under a sequence of keys in given Aeson.
--   If not possible returns JsonDecodeError.
getFieldNested :: Array String -> Aeson -> Either JsonDecodeError Aeson
getFieldNested keys asn@(Aeson { numberIndex, patchedJson: AesonPatchedJson pjson }) =
  note (UnexpectedValue $ toStringifiedNumbersJson asn) $
    mkAeson <$> (foldM lookup pjson keys :: Maybe Json)
  where
  lookup :: Json -> String -> Maybe Json
  lookup j lbl = caseJsonObject Nothing (FO.lookup lbl) j

  mkAeson :: Json -> Aeson
  mkAeson json = Aeson { numberIndex, patchedJson: AesonPatchedJson json }

infix 7 getField as .:

-- TODO: add getFieldOptional if ever needed.

foreign import parseJsonExtractingIntegers
  :: String
  -> { patchedPayload :: String, numberIndex :: NumberIndex }

-- | Ignore numeric index and reuse Argonaut decoder.
decodeAesonViaJson
  :: forall (a :: Type). DecodeJson a => Aeson -> Either JsonDecodeError a
decodeAesonViaJson (Aeson { patchedJson: AesonPatchedJson j }) = decodeJson j

parseJsonStringToAeson :: String -> Either JsonDecodeError Aeson
parseJsonStringToAeson payload = do
  let { patchedPayload, numberIndex } = parseJsonExtractingIntegers payload
  patchedJson <- note MissingValue $ hush $ AesonPatchedJson <$> jsonParser patchedPayload
  pure $ Aeson { numberIndex, patchedJson }

decodeAesonString
  :: forall (a :: Type). DecodeAeson a => String -> Either JsonDecodeError a
decodeAesonString = parseJsonStringToAeson >=> decodeAeson

-- | Recodes Json to Aeson.
--   NOTE. The operation is costly as its stringifies given Json
--         and reparses resulting string as Aeson.
jsonToAeson :: Json -> Aeson
jsonToAeson = stringify >>> decodeAesonString >>> fromRight shouldNotHappen
  where
  -- valid json should always decode without errors
  shouldNotHappen = undefined
