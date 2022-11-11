-- | Utils for QuickCheck
module Ctl.Internal.QuickCheck
  ( unMaybeGen
  ) where

import Prelude

import Data.Maybe (Maybe, fromJust, isJust)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Gen (Gen, resize, sized)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust x = unsafePartial $ fromJust x

-- | Run Gen (Maybe a) until it generates Just a, and return a
unMaybeGen :: forall a. Gen (Maybe a) -> Gen a
unMaybeGen gen = do
  mx <- gen
  if isJust mx then pure $ unsafeFromJust mx
  else sized (\n -> resize (n + 1) (unMaybeGen gen))
