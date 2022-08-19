module Contract.Log
  ( logTrace
  , logTrace'
  , logDebug
  , logDebug'
  , logInfo
  , logInfo'
  , logWarn
  , logWarn'
  , logError
  , logError'
  , logAeson
  , logAesonTrace
  , logAesonDebug
  , logAesonInfo
  , logAesonWarn
  , logAesonError
  ) where

import Prelude

import Aeson (class EncodeAeson, encodeAeson, stringifyAeson)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Data.Log.Tag (TagSet)
import Data.Map as Map

logTrace
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logTrace = Logger.trace

logDebug
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logDebug = Logger.debug

logInfo
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logInfo = Logger.info

logWarn
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logWarn = Logger.warn

logError
  :: forall (m :: Type -> Type). MonadLogger m => TagSet -> String -> m Unit
logError = Logger.error

logTrace'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logTrace' = Logger.trace Map.empty

logDebug'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logDebug' = Logger.debug Map.empty

logInfo'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logInfo' = Logger.info Map.empty

logWarn'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logWarn' = Logger.warn Map.empty

logError'
  :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logError' = Logger.error Map.empty

-- | Log JSON representation of a data structure
logAeson
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => (String -> m Unit)
  -- ^ Logging function to use
  -> a
  -- ^ Data structure to output
  -> m Unit
logAeson logger = logger <<< stringifyAeson <<< encodeAeson

logAesonTrace
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonTrace = logAeson logTrace'

logAesonDebug
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonDebug = logAeson logDebug'

logAesonInfo
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonInfo = logAeson logInfo'

logAesonWarn
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonWarn = logAeson logWarn'

logAesonError
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadLogger m
  => EncodeAeson a
  => a
  -- ^ Data structure to output
  -> m Unit
logAesonError = logAeson logError'
