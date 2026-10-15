module Test.Ctl.Utils
  ( aesonRoundTrip
  , assertTrue
  , assertTrue_
  , errEither
  , errMaybe
  , measure
  , measure'
  , measureWithTimeout
  , readAeson
  , toFromAesonTest
  , toFromAesonTestWith
  , fromBytesEffect
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError
  , decodeAeson
  , encodeAeson
  , parseJsonStringToAeson
  )
import Cardano.Data.Lite (class IsBytes, fromBytes)
import Cardano.Data.Lite.Internal (class IsCsl)
import Data.ByteArray (ByteArray)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(Right), either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (class Duration, Milliseconds, Seconds)
import Data.Time.Duration (fromDuration, toDuration) as Duration
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw, throwException)
import Effect.Now (now)
import Mote (test)
import Mote.TestPlanM (TestPlanM)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Test.Spec.Assertions (shouldEqual)

measure :: forall (m :: Type -> Type) (a :: Type). MonadEffect m => m a -> m a
measure = measure' (Nothing :: Maybe Seconds)

measureWithTimeout
  :: forall (m :: Type -> Type) (d :: Type) (a :: Type)
   . MonadEffect m
  => Duration d
  => d
  -> m a
  -> m a
measureWithTimeout timeout = measure' (Just timeout)

measure'
  :: forall (m :: Type -> Type) (d :: Type) (a :: Type)
   . MonadEffect m
  => Duration d
  => Maybe d
  -> m a
  -> m a
measure' timeout action =
  getNowMs >>= \startTime -> action >>= \result -> getNowMs >>= \endTime ->
    liftEffect do
      let
        duration :: Milliseconds
        duration = wrap (endTime - startTime)

        durationSeconds :: Seconds
        durationSeconds = Duration.toDuration duration

      case timeout of
        Just timeout' | duration > Duration.fromDuration timeout' -> do
          let msg = "Timeout exceeded, execution time: " <> show durationSeconds
          throwException (error msg)
        _ ->
          log ("---\nExecution time: " <> show durationSeconds)
      pure result
  where
  getNowMs :: m Number
  getNowMs = unwrap <<< unInstant <$> liftEffect now

-- | Test a boolean value, throwing the provided string as an error if `false`
assertTrue
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => String
  -> Boolean
  -> m Unit
assertTrue msg b = unless b $ liftEffect $ throwException $ error msg

assertTrue_
  :: forall (m :: Type -> Type)
   . Applicative m
  => MonadEffect m
  => Boolean
  -> m Unit
assertTrue_ = assertTrue "Boolean test failed"

errEither
  :: forall (m :: Type -> Type) (a :: Type) (e :: Type)
   . MonadEffect m
  => Show e
  => Either e a
  -> m a
errEither = either (liftEffect <<< throw <<< show) pure

errMaybe
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadEffect m
  => String
  -> Maybe a
  -> m a
errMaybe msg = maybe (liftEffect $ throw msg) pure

toFromAesonTest
  :: forall (a :: Type)
   . Eq a
  => DecodeAeson a
  => EncodeAeson a
  => Show a
  => String
  -> a
  -> TestPlanM (Aff Unit) Unit
toFromAesonTest desc x = test desc $ aesonRoundTrip x `shouldEqual` Right x

toFromAesonTestWith
  :: forall (a :: Type)
   . Eq a
  => DecodeAeson a
  => EncodeAeson a
  => Show a
  => String
  -> (a -> a)
  -> a
  -> TestPlanM (Aff Unit) Unit
toFromAesonTestWith desc transform x =
  test desc $ aesonRoundTripWith transform x `shouldEqual` Right x

aesonRoundTripWith
  :: forall (a :: Type)
   . Eq a
  => Show a
  => DecodeAeson a
  => EncodeAeson a
  => (a -> a)
  -> a
  -> Either JsonDecodeError a
aesonRoundTripWith transform = decodeAeson <<< encodeAeson <<< transform

aesonRoundTrip
  :: forall (a :: Type)
   . Eq a
  => Show a
  => DecodeAeson a
  => EncodeAeson a
  => a
  -> Either JsonDecodeError a
aesonRoundTrip = aesonRoundTripWith identity

readAeson :: forall (m :: Type -> Type). MonadEffect m => FilePath -> m Aeson
readAeson = errEither <<< parseJsonStringToAeson
  <=< liftEffect <<< readTextFile UTF8

fromBytesEffect
  :: forall (a :: Type). IsCsl a => IsBytes a => ByteArray -> Effect a
fromBytesEffect bytes =
  case fromBytes bytes of
    Nothing -> throw "from_bytes() call failed"
    Just a -> pure a
