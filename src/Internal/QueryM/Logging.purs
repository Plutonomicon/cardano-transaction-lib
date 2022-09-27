module Ctl.Internal.QueryM.Logging
  ( setupLogs
  ) where

import Prelude

import Ctl.Internal.QueryM (Logger, mkLogger)
import Data.List (List(Cons, Nil))
import Data.List as List
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

-- | Setup internal machinery for log suppression.
setupLogs
  :: LogLevel
  -> Maybe (Message -> Aff Unit)
  -> Effect
       { addLogEntry :: Message -> Effect Unit
       , logger :: LogLevel -> String -> Effect Unit
       , printLogs :: Effect Unit
       , suppressedLogger :: LogLevel -> String -> Effect Unit
       }
setupLogs logLevel customLogger = do
  -- Keep track of logs to show them only on error if `suppressLogs: true`
  logsRef <- liftEffect $ Ref.new Nil
  let
    -- Logger that stores a message in the queue
    addLogEntry :: Message -> Effect Unit
    addLogEntry msg = do
      Ref.modify_ (Cons msg) logsRef

    -- Logger that is used to actually print messages
    logger :: Logger
    logger = mkLogger logLevel customLogger

    -- Looger that adds message to the queue, respecting LogLevel
    suppressedLogger :: Logger
    suppressedLogger = mkLogger logLevel
      (Just $ liftEffect <<< addLogEntry)

    -- Print suppressed logs
    printLogs :: Effect Unit
    printLogs = do
      logs <- List.reverse <$> Ref.read logsRef
      Ref.write Nil logsRef
      for_ logs \logEntry -> do
        logger logEntry.level logEntry.message

  pure { addLogEntry, logger, printLogs, suppressedLogger }
