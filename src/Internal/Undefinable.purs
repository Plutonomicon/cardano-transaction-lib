module Ctl.Internal.Undefinable
  ( fromUndefinable
  , toUndefinable
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing, Just))
import Undefined (undefined)

fromUndefinable :: forall a. Eq a => a -> Maybe a
fromUndefinable x | isUndefined x = Nothing
fromUndefinable x | otherwise = Just x

toUndefinable :: forall a. Maybe a -> a
toUndefinable (Just x) = x
toUndefinable Nothing = undefined

-- Checking with `x == undefined` does not work for row types
foreign import isUndefined :: forall a. a -> Boolean
