module Helpers
  ( fromJustEff
  , parseJsonStringifyNumbers
  , jsonTurnNumbersToStrings
  , liftEither
  , fromRightEff
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut
  ( JsonDecodeError
  , Json
  , parseJson
  )
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either, either)
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

liftEither
  :: forall (a :: Type) (e :: Type) (m :: Type -> Type)
   . MonadError e m
  => Either e a
  -> m a
liftEither = either throwError pure

fromRightEff :: forall (a :: Type) (e :: Type). Show e => Either e a -> Effect a
fromRightEff = either (throw <<< show) pure
