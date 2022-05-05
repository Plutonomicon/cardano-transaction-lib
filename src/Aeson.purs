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
module Aeson
  ( NumberIndex
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , Aeson
  , (.:)
  , (.:?)
  , AesonCases
  , caseAeson
  , caseAesonArray
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonObject
  , caseAesonString
  , caseAesonNumber
  , caseAesonUInt
  , caseAesonBigInt
  , constAesonCases
  , decodeAeson
  , decodeAesonField
  , decodeJsonString
  , decodeAesonViaJson
  , gDecodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , getNumberIndex
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , toObject
  , toStringifiedNumbersJson
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
      ( TypeMismatch
      , AtKey
      , MissingValue
      , UnexpectedValue
      )
  , caseJson
  , caseJsonObject
  , decodeJson
  , fromArray
  , fromObject
  , jsonNull
  , stringify
  , isNull
  )
import Data.Argonaut.Encode.Encoders (encodeBoolean, encodeString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (foldM)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), fromRight, note)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, for)
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(Proxy))
import Untagged.Union (class InOneOf, type (|+|), asOneOf)

-- | A piece of JSON where all numbers are replaced with their indexes
newtype AesonPatchedJson = AesonPatchedJson Json

-- | A piece of JSON where all numbers are extracted into `NumberIndex`.
newtype Aeson = Aeson
  { patchedJson :: AesonPatchedJson, numberIndex :: NumberIndex }

instance Eq Aeson where
  eq a b = stringifyAeson a == stringifyAeson b

instance Show Aeson where
  show = stringifyAeson

-- | A list of numbers extracted from Json, as they appear in the payload.
type NumberIndex = Array String

class DecodeAeson (a :: Type) where
  decodeAeson :: Aeson -> Either JsonDecodeError a

-------- Parsing: String -> Aeson --------

foreign import parseJsonExtractingIntegers
  :: String
  -> { patchedPayload :: String, numberIndex :: NumberIndex }

parseJsonStringToAeson :: String -> Either JsonDecodeError Aeson
parseJsonStringToAeson payload = do
  let { patchedPayload, numberIndex } = parseJsonExtractingIntegers payload
  patchedJson <- lmap (const MissingValue) $ AesonPatchedJson <$> jsonParser
    patchedPayload
  pure $ Aeson { numberIndex, patchedJson }

-------- Stringifying: Aeson -> String

foreign import stringifyAeson_ :: NumberIndex -> AesonPatchedJson -> String

stringifyAeson :: Aeson -> String
stringifyAeson (Aeson { patchedJson, numberIndex }) = stringifyAeson_
  numberIndex
  patchedJson

-------- Json <-> Aeson --------

-- | Replaces indexes in the Aeson's payload with stringified
-- | numbers from numberIndex.
-- | Given original payload of: `{"a": 10}`
-- | The result will be an Json object representing: `{"a": "10"}`
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

-- | Recodes Json to Aeson.
-- | NOTE. The operation is costly as its stringifies given Json
-- |       and reparses resulting string as Aeson.
jsonToAeson :: Json -> Aeson
jsonToAeson = stringify >>> decodeJsonString >>> fromRight shouldNotHappen
  where
  -- valid json should always decode without errors
  shouldNotHappen = undefined

getNumberIndex :: Aeson -> NumberIndex
getNumberIndex (Aeson { numberIndex }) = numberIndex

-------- Aeson manipulation and field accessors --------

getField
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError a
getField aesonObject field = getField' decodeAeson aesonObject field
  where
  -- | Adapted from `Data.Argonaut.Decode.Decoders`
  getField'
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError a
  getField' decoder obj str =
    maybe
      (Left $ AtKey str MissingValue)
      (lmap (AtKey str) <<< decoder)
      (FO.lookup str obj)

infix 7 getField as .:

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `getFieldOptional'` (`.:?`) instead.
getFieldOptional
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional = getFieldOptional_ decodeAeson
  where
  getFieldOptional_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional_ decoder obj str =
    maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
    where
    decode = lmap (AtKey str) <<< decoder

infix 7 getFieldOptional as .:!

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getField` (`.:`) instead.
getFieldOptional'
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' = getFieldOptional'_ decodeAeson
  where
  getFieldOptional'_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional'_ decoder obj str =
    maybe (pure Nothing) decode (FO.lookup str obj)
    where
    decode aeson@(Aeson { patchedJson: AesonPatchedJson json }) =
      if isNull json then
        pure Nothing
      else
        Just <$> (lmap (AtKey str) <<< decoder) aeson

infix 7 getFieldOptional' as .:?

-- | Returns an Aeson available under a sequence of keys in given Aeson.
-- | If not possible returns JsonDecodeError.
getNestedAeson :: Aeson -> Array String -> Either JsonDecodeError Aeson
getNestedAeson
  asn@(Aeson { numberIndex, patchedJson: AesonPatchedJson pjson })
  keys =
  note (UnexpectedValue $ toStringifiedNumbersJson asn) $
    mkAeson <$> (foldM lookup pjson keys :: Maybe Json)
  where
  lookup :: Json -> String -> Maybe Json
  lookup j lbl = caseJsonObject Nothing (FO.lookup lbl) j

  mkAeson :: Json -> Aeson
  mkAeson json = Aeson { numberIndex, patchedJson: AesonPatchedJson json }

-- | Utility abbrevation. See `caseAeson` for an example usage.
type AesonCases a =
  { caseNull :: Unit -> a
  , caseBoolean :: Boolean -> a
  , caseNumber :: String -> a
  , caseString :: String -> a
  , caseArray :: Array Aeson -> a
  , caseObject :: Object Aeson -> a
  }

caseAeson
  :: forall (a :: Type)
   . AesonCases a
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

constAesonCases :: forall (a :: Type). a -> AesonCases a
constAesonCases v =
  { caseObject: c
  , caseNull: c
  , caseBoolean: c
  , caseString: c
  , caseNumber: c
  , caseArray: c
  }
  where
  c :: forall (b :: Type). b -> a
  c = const v

toObject :: Aeson -> Maybe (Object Aeson)
toObject =
  caseAeson $ constAesonCases Nothing # _ { caseObject = Just }

caseAesonObject :: forall (a :: Type). a -> (Object Aeson -> a) -> Aeson -> a
caseAesonObject def f = caseAeson (constAesonCases def # _ { caseObject = f })

caseAesonString :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonString def f = caseAeson (constAesonCases def # _ { caseString = f })

caseAesonArray :: forall (a :: Type). a -> (Array Aeson -> a) -> Aeson -> a
caseAesonArray def f = caseAeson (constAesonCases def # _ { caseArray = f })

caseAesonBoolean :: forall (a :: Type). a -> (Boolean -> a) -> Aeson -> a
caseAesonBoolean def f = caseAeson (constAesonCases def # _ { caseBoolean = f })

-- | String representation is used to allow users to choose numeric representation downstream.
caseAesonNumber :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonNumber def f = caseAeson (constAesonCases def # _ { caseNumber = f })

-- | `caseAesonNumber` specialized to `UInt` (fails if no parse)
caseAesonUInt :: forall (a :: Type). a -> (UInt -> a) -> Aeson -> a
caseAesonUInt def f = caseAesonNumber def \str ->
  case UInt.fromString str of
    Nothing -> def
    Just res -> f res

-- | `caseAesonNumber` specialized to `BigInt` (fails if no parse)
caseAesonBigInt :: forall (a :: Type). a -> (BigInt -> a) -> Aeson -> a
caseAesonBigInt def f = caseAesonNumber def \str ->
  case BigInt.fromString str of
    Nothing -> def
    Just res -> f res

caseAesonNull :: forall (a :: Type). a -> (Unit -> a) -> Aeson -> a
caseAesonNull def f = caseAeson (constAesonCases def # _ { caseNull = f })

-------- Decode helpers --------

-- | Ignore numeric index and reuse Argonaut decoder.
decodeAesonViaJson
  :: forall (a :: Type). DecodeJson a => Aeson -> Either JsonDecodeError a
decodeAesonViaJson (Aeson { patchedJson: AesonPatchedJson j }) = decodeJson j

-- | Decodes a value encoded as JSON via Aeson decoding algorithm.
decodeJsonString
  :: forall (a :: Type). DecodeAeson a => String -> Either JsonDecodeError a
decodeJsonString = parseJsonStringToAeson >=> decodeAeson

-------- DecodeAeson instances --------

decodeIntegral
  :: forall a. (String -> Maybe a) -> Aeson -> Either JsonDecodeError a
decodeIntegral parse aeson@(Aeson { numberIndex }) = do
  -- Numbers are replaced by their index in the array.
  ix <- decodeAesonViaJson aeson
  numberStr <- note MissingValue (numberIndex Array.!! ix)
  note (TypeMismatch $ "Couldn't parse to integral: " <> numberStr)
    (parse numberStr)

instance DecodeAeson UInt where
  decodeAeson = decodeIntegral UInt.fromString

instance DecodeAeson Int where
  decodeAeson = decodeIntegral Int.fromString

instance DecodeAeson BigInt where
  decodeAeson = decodeIntegral BigInt.fromString

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

instance
  ( GDecodeAeson row list
  , RL.RowToList row list
  ) =>
  DecodeAeson (Record row) where
  decodeAeson json =
    case toObject json of
      Just object -> gDecodeAeson object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

else instance
  ( InOneOf b a b
  , DecodeAeson a
  , DecodeAeson b
  ) =>
  DecodeAeson (a |+| b) where
  decodeAeson j =
    asOneOf <$> (decodeAeson j :: Either JsonDecodeError a)
      <|> asOneOf <$> (decodeAeson j :: Either JsonDecodeError b)

else instance
  ( Traversable t
  , DecodeAeson a
  , DecodeJson (t Json)
  ) =>
  DecodeAeson (t a) where
  decodeAeson (Aeson { numberIndex, patchedJson: AesonPatchedJson pJson }) = do
    jsons :: t _ <- map AesonPatchedJson <$> decodeJson pJson
    for jsons (\patchedJson -> decodeAeson (Aeson { patchedJson, numberIndex }))

class
  GDecodeAeson (row :: Row Type) (list :: RL.RowList Type)
  | list -> row where
  gDecodeAeson
    :: forall proxy
     . Object Aeson
    -> proxy list
    -> Either JsonDecodeError (Record row)

instance GDecodeAeson () RL.Nil where
  gDecodeAeson _ _ = Right {}

instance
  ( DecodeAesonField value
  , GDecodeAeson rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  ) =>
  GDecodeAeson row (RL.Cons field value tail) where
  gDecodeAeson object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeAesonField fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeAeson object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue

class DecodeAesonField a where
  decodeAesonField :: Maybe Aeson -> Maybe (Either JsonDecodeError a)

instance DecodeAeson a => DecodeAesonField (Maybe a) where
  decodeAesonField = Just <<< maybe (Right Nothing) decodeAeson

else instance DecodeAeson a => DecodeAesonField a where
  decodeAesonField j = decodeAeson <$> j
