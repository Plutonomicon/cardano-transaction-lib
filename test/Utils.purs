module Test.Utils (unsafeCall, assertTrue, assertTrue_, errMaybe) where

import Control.Alternative (pure)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException, throw)
import TestM (TestPlanM)
import Type.Prelude (Proxy)

foreign import unsafeCall :: forall a b. Proxy b -> String -> a -> b

-- | Make boolean a test
assertTrue :: String -> Boolean -> TestPlanM Unit
assertTrue msg b =
  if b then pure unit
  else (liftEffect $ throwException $ error msg)

assertTrue_ :: Boolean -> TestPlanM Unit
assertTrue_ = assertTrue "Boolean test failed"

errMaybe :: forall m a. MonadEffect m => String -> Maybe a -> m a
errMaybe msg = case _ of
  Nothing -> liftEffect $ throw msg
  Just res -> pure res
