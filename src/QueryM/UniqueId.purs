module QueryM.UniqueId (ListenerId, uniqueId) where

import Effect (Effect)

-- | Creates a unique id prefixed by its argument
foreign import uniqueId :: String -> Effect String

-- | A unique request ID used for dispatching
type ListenerId = String
