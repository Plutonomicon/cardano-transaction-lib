module FfiHelpers
  ( MaybeFfiHelper
  , maybeFfiHelper
  , ContainerHelper
  , containerHelper
  ) where

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))

type MaybeFfiHelper =
  { nothing :: forall (x :: Type). Maybe x
  , just :: forall (x :: Type). x -> Maybe x
  , from :: forall (x :: Type). x -> Maybe x -> x
  }

maybeFfiHelper :: MaybeFfiHelper
maybeFfiHelper = { nothing: Nothing, just: Just, from: fromMaybe }

foreign import data ContainerHelper :: Type

foreign import _containerHelper
  :: (forall a. Tuple a a -> Array a) -> ContainerHelper

containerHelper :: ContainerHelper
containerHelper = _containerHelper untuple

untuple :: forall a. Tuple a a -> Array a
untuple (Tuple a b) = [ a, b ]
