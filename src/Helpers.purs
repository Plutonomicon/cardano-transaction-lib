module Helpers
  ( fromJustEff
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
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Exception (throw)

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
