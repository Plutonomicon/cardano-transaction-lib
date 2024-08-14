module Ctl.Examples.KeyWallet.Internal.HtmlForm
  ( Form
  , Log
  , Unlock
  , levelColor
  , levelName
  , logError
  , mkForm
  ) where

import Contract.Prelude

import Data.Log.Level (LogLevel(Trace, Debug, Warn, Info, Error))
import Effect.Exception (Error)

type Form =
  { walletSeed :: String
  }

type Log = String -> String -> Effect Unit

type Unlock = Effect Unit

foreign import mkForm :: (Form -> Log -> Unlock -> Effect Unit) -> Effect Unit

foreign import logError :: Error -> Effect Unit

levelName :: LogLevel -> String
levelName Trace = "TRACE"
levelName Debug = "DEBUG"
levelName Info = "INFO"
levelName Warn = "WARN"
levelName Error = "ERROR"

levelColor :: LogLevel -> String
levelColor Warn = "gold"
levelColor Error = "crimson"
levelColor _ = "black"
