module Ctl.Internal.Contract.AppInstance (AppInstanceId, newAppInstanceId) where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)

type AppInstanceId = Int

newAppInstanceId :: Effect AppInstanceId
newAppInstanceId = randomInt bottom top
