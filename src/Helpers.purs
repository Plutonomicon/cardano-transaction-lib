module Helpers
  ( fromJustEff
  , parseJsonStringifyNumbers
  , jsonTurnNumbersToStrings
  ) where

import Prelude

import Data.Argonaut
  ( JsonDecodeError
  , Json
  , parseJson
  )
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Exception (throw)

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

-- | Throws provided error on `Nothing`
fromJustEff :: forall (a :: Type). String -> Maybe a -> Effect a
fromJustEff e = case _ of
  Nothing -> throw e
  Just x -> pure x
