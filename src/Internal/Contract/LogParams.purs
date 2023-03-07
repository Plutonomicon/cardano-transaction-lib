module Ctl.Internal.Contract.LogParams (LogParams) where

import Prelude

import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type LogParams rest =
  { logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  | rest
  }
