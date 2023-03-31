module Ctl.Internal.UsedTxOuts.CleanupBeforeUnload
  ( attachCleanupHandler
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)

attachCleanupHandler :: Effect Unit -> Effect (Maybe (Effect Unit))
attachCleanupHandler = _attachCleanupHandler Nothing Just

foreign import _attachCleanupHandler
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Effect Unit
  -> Effect (Maybe (Effect Unit))
