module Helpers(parseJsonStringifyNumbers, jsonTurnNumbersToStrings) where

import Data.Either (Either)
import Data.Argonaut (JsonDecodeError, Json, parseJson)

import Prelude

-- | Assuming a valid JSON string in its input, the function will quote each
-- | value that would otherwise be parsed by JSON.parse() as a number.
-- | NOTE it discards whitespaces outside of the would be json strings
foreign import jsonTurnNumbersToStrings :: String -> String


-- | Parse JSON from string. It parses numbers as strings.
parseJsonStringifyNumbers :: String -> Either JsonDecodeError Json
parseJsonStringifyNumbers s = do
  _ <- parseJson s
  -- valid json ensured at this point
  parseJson $ jsonTurnNumbersToStrings s
