module Test.Utils
  ( aesonRoundTrip
  , assertTrue
  , assertTrue_
  , errMaybe
  , errEither
  , interpret
  , interpret'
  , toFromAesonTest
  , unsafeCall
  , readAeson
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
import Data.Either (Either(Right), either)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (wrap)
import Effect.Aff (Aff, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException, throw)
import Mote (Plan, foldPlan, planT, test)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as SpecRunner
import Test.Spec.Runner (defaultConfig, runSpec')
import TestM (TestPlanM)
import Type.Proxy (Proxy)

foreign import unsafeCall
  :: forall (a :: Type) (b :: Type). Proxy b -> String -> a -> b

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret' :: SpecRunner.Config -> TestPlanM Unit -> Aff Unit
interpret' config spif = do
  plan <- planT spif
  runSpec' defaultConfig { timeout = Just (wrap 10000.0) } [ consoleReporter ] $
    go plan
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftAff x.value)
      pending
      (\x -> describe x.label $ go x.value)
      sequence_

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM Unit -> Aff Unit
interpret = interpret' defaultConfig

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
  -> TestPlanM Unit
toFromAesonTest desc x = test desc $ aesonRoundTrip x `shouldEqual` Right x

aesonRoundTrip
  :: forall (a :: Type)
   . Eq a
  => Show a
  => DecodeAeson a
  => EncodeAeson a
  => a
  -> Either JsonDecodeError a
aesonRoundTrip = decodeAeson <<< encodeAeson

readAeson :: forall (m :: Type -> Type). MonadEffect m => FilePath -> m Aeson
readAeson = errEither <<< parseJsonStringToAeson
  <=< liftEffect <<< readTextFile UTF8

