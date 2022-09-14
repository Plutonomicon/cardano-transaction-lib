module Test.CTL.Utils
  ( module ExportSeconds
  , aesonRoundTrip
  , assertTrue
  , assertTrue_
  , errEither
  , errMaybe
  , interpret
  , interpretWithConfig
  , interpretWithTimeout
  , measure
  , measure'
  , measureWithTimeout
  , readAeson
  , toFromAesonTest
  , toFromAesonTestWith
  , unsafeCall
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
import Data.Const (Const)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(Right), either)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (class Duration, Milliseconds, Seconds)
import Data.Time.Duration (Seconds(Seconds)) as ExportSeconds
import Data.Time.Duration (fromDuration, toDuration) as Duration
import Effect.Aff (Aff, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw, throwException)
import Effect.Now (now)
import Mote (Plan, foldPlan, planT, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Test.CTL.TestM (TestPlanM)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Spec.Runner as SpecRunner
import Type.Proxy (Proxy)

foreign import unsafeCall
  :: forall (a :: Type) (b :: Type). Proxy b -> String -> a -> b

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM (Aff Unit) Unit -> Aff Unit
interpret = interpretWithConfig defaultConfig { timeout = Just (wrap 50000.0) }

interpretWithTimeout
  :: Maybe Milliseconds -> TestPlanM (Aff Unit) Unit -> Aff Unit
interpretWithTimeout timeout spif = do
  interpretWithConfig (defaultConfig { timeout = timeout }) spif

interpretWithConfig
  :: SpecRunner.Config -> TestPlanM (Aff Unit) Unit -> Aff Unit
interpretWithConfig config spif = do
  plan <- planT spif
  runSpec' config [ consoleReporter ] $ planToSpec plan

planToSpec :: Plan (Const Void) (Aff Unit) -> Spec Unit
planToSpec =
  foldPlan
    (\x -> it x.label $ liftAff x.value)
    pending
    (\x -> describe x.label $ planToSpec x.value)
    sequence_

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
