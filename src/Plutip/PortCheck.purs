module Plutip.PortCheck
  ( isPortAvailable
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.UInt (UInt, toInt)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import _isPortAvailable :: Int -> Effect (Promise Boolean)

isPortAvailable :: UInt -> Aff Boolean
isPortAvailable = toAffE <<< _isPortAvailable <<< toInt
