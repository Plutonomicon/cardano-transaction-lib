module Test.Utils (unsafeCall, assertTrue, assertTrue_, errMaybe) where

import Control.Alternative (class Applicative, pure)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Effect.Aff (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException, throw)
import Type.Prelude (Proxy)

foreign import unsafeCall :: forall a b. Proxy b -> String -> a -> b

-- | Make boolean a test
assertTrue :: forall (m :: Type -> Type). Applicative m => MonadEffect m => String -> Boolean -> m Unit
assertTrue msg b =
  if b then pure unit
  else (liftEffect $ throwException $ error msg)

assertTrue_ :: forall (m :: Type -> Type). Applicative m => MonadEffect m => Boolean -> m Unit
assertTrue_ = assertTrue "Boolean test failed"

errMaybe :: forall (m :: Type -> Type) (a :: Type). MonadEffect m => String -> Maybe a -> m a
errMaybe msg = case _ of
  Nothing -> liftEffect $ throw msg
  Just res -> pure res
