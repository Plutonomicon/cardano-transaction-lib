module Ctl.Internal.UsedTxOut.LocalStorage where

import Ctl.Internal.UsedTxOuts.Storage (Storage)
import Data.Maybe (Maybe(Just, Nothing))

localStorage :: Storage
localStorage = _localStorage Nothing Just

foreign import _localStorage
  :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Storage
