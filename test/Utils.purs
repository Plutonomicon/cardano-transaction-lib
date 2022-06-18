module Test.Utils
  ( aesonRoundTrip
  , assertTrue
  , assertTrue_
  , errMaybe
  , interpret
  , interpret'    
  , toFromAesonTest
  , unsafeCall
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError
  , decodeAeson
  , encodeAeson
  )
import Data.Const (Const)
import Data.Either (Either(Right))
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException, throw)
import Mote (Plan, foldPlan, planT, test)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.Spec.Runner as SpecRunner
import TestM (TestPlanM)
import Type.Proxy (Proxy)

foreign import unsafeCall :: forall a b. Proxy b -> String -> a -> b

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret' :: SpecRunner.Config -> TestPlanM Unit -> Aff Unit
interpret' config spif = do
  plan <- planT spif
  runSpec'  config [ consoleReporter ] $ go plan
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftAff x.value)
      (const $ pure unit)
      (\x -> describe x.label $ go x.value)
      sequence_

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM Unit -> Aff Unit
interpret = interpret' SpecRunner.defaultConfig

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

errMaybe
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadEffect m
  => String
  -> Maybe a
  -> m a
errMaybe msg = case _ of
  Nothing -> liftEffect $ throw msg
  Just res -> pure res

toFromAesonTest
  :: forall (a :: Type)
   . Eq a
  => Show a
  => DecodeAeson a
  => EncodeAeson a
  => a
  -> TestPlanM Unit
toFromAesonTest x = test (show x) $ do
  let
    (xOrErr :: Either JsonDecodeError a) =
      aesonRoundTrip x
  xOrErr `shouldEqual` Right x

aesonRoundTrip
  :: forall (a :: Type)
   . Eq a
  => Show a
  => DecodeAeson a
  => EncodeAeson a
  => a
  -> Either JsonDecodeError a
aesonRoundTrip = decodeAeson <<< encodeAeson
