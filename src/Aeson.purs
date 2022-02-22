-- | Argonaut can't decode long integers the way Aeson encodes them: they
-- | lose precision on the stage of `JSON.parse` call, which we can't really
-- | control. This module is a hacky solution allowing us to preserve long
-- | integers while decoding.
module Aeson where

import Prelude
import Data.Tuple
import Data.Tuple.Nested
import Data.Argonaut
import Data.Maybe
import Data.Array as Array
import Data.Either
import Data.Int as Int
import Data.Argonaut.Parser (jsonParser)
import Data.Traversable

type NumberIndex = Array String

class DecodeAeson a where
  decodeAeson :: NumberIndex -> Json -> Maybe a

instance DecodeAeson Int where
  decodeAeson index json = do
    ix <- decodeAesonViaJson' json
    index Array.!! ix >>= Int.fromString

instance DecodeAeson String where
  decodeAeson = decodeAesonViaJson

instance DecodeAeson Number where
  decodeAeson = decodeAesonViaJson

instance (Traversable t, DecodeAeson a, DecodeJson (t Json)) => DecodeAeson (t a) where
  decodeAeson ix json = do
    jsons :: t _ <- hush $ decodeJson json
    for jsons (decodeAeson ix)

foreign import parseJsonExtractingIntegers
  :: (forall a b. a -> b -> Tuple a b) -> String -> String /\ Array String

decodeAesonViaJson :: forall a. DecodeJson a => NumberIndex -> Json -> Maybe a
decodeAesonViaJson _ = hush <<< decodeJson

decodeAesonViaJson' :: forall a. DecodeJson a => Json -> Maybe a
decodeAesonViaJson' = decodeAesonViaJson []

decodeAesonString :: forall a. DecodeAeson a => String -> Maybe a
decodeAesonString payload = do
  payloadJson <- hush $ jsonParser patchedPayload
  decodeAeson index payloadJson
  where
  patchedPayload /\ index = parseJsonExtractingIntegers Tuple payload
