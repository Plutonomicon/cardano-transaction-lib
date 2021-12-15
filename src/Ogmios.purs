module Ogmios where

import Prelude
-- foreign import getContext :: ConnectionConfig -> Effect (Promise InteractionContext)

data ConnectionConfig = ConnectionConfig {
  host :: String,
  port :: Int,
  tls :: Boolean
  -- maxPayload: number
}
