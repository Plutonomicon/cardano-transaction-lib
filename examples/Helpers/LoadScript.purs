module Ctl.Examples.Helpers.LoadScript
  ( loadScript
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import _loadScript :: String -> Effect (Promise String)

loadScript :: String -> Aff String
loadScript filepath = toAffE $ _loadScript filepath
