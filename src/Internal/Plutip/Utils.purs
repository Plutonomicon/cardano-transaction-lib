module Ctl.Internal.Plutip.Utils
  ( mkDirIfNotExists
  , runCleanup
  , tmpdir
  , cleanupOnExit
  , handleLines
  , EventSource(EventSource)
  , onLine
  , makeEventSource
  , narrowEventSource
  , waitForEvent
  , addCleanup
  , after
  , suppressAndLogErrors
  ) where

import Contract.Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Data.Array as Array
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import Node.ReadLine as RL
import Node.Stream (Readable)

-- TODO: remove this function when PS bindings for os.tmpdir are available.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/726
foreign import tmpdir :: Effect String

suppressAndLogErrors
  :: forall a m. MonadEffect m => MonadError Error m => m Unit -> m Unit
suppressAndLogErrors = flip catchError $ message
  >>> append "An error occured and suppressed: "
  >>> log

mkDirIfNotExists :: FilePath -> Effect Unit
mkDirIfNotExists dirName = do
  exists <- FS.exists dirName
  unless exists $ FS.mkdir dirName

runCleanup :: Ref (Array (Aff Unit)) -> Aff Unit
runCleanup cleanupRef = do
  cleanups <- liftEffect $ Ref.read cleanupRef
  sequence_ (try <$> cleanups)

waitForBeforeExit :: Aff Unit
waitForBeforeExit = Aff.makeAff \cont -> do
  isCanceledRef <- Ref.new false
  let cancel = Ref.write false isCanceledRef
  Process.onBeforeExit do
    isCanceled <- Ref.read isCanceledRef
    unless isCanceled do
      cont $ Right unit
  pure $ Aff.Canceler $ const $ liftEffect cancel

handleLines
  :: forall a
   . Readable a
  -> (String -> Effect Unit)
  -> Effect { cancel :: Effect Unit }
handleLines readable handler = do
  EventSource { subscribe, cancel } <- onLine readable Just
  _ <- subscribe $ handler <<< _.event
  pure { cancel }

newtype EventSource b = EventSource
  { subscribe ::
      ({ unsubscribe :: Effect Unit, event :: b } -> Effect Unit)
      -> Effect { unsubscribe :: Effect Unit }
  , cancel :: Effect Unit
  }

-- | Waits for any event. Note, if the event source throws an async error, any joining process dies.
waitForEvent :: forall a. EventSource a -> Aff a
waitForEvent (EventSource { subscribe }) = Aff.makeAff \cont -> do
  canceler <- makeCanceler
  { unsubscribe } <- subscribe \{ unsubscribe, event } -> do
    unsubscribe
    canceler.isCanceled >>= flip unless do
      cont $ Right event
  pure $ Aff.Canceler $ const $ liftEffect do
    unsubscribe
    canceler.cancel

onLine
  :: forall a b
   . Readable a
  -> (String -> Maybe b)
  -> Effect (EventSource b)
onLine readable =
  map _.eventSource <<< makeEventSource \cont -> do
    interface <- RL.createInterface readable mempty
    flip RL.setLineHandler interface cont

-- | Create an event source based on another event source, but
-- with smaller variety of events.
narrowEventSource
  :: forall a b
   . (a -> Maybe b)
  -> EventSource a
  -> Effect (EventSource b)
narrowEventSource filter (EventSource { subscribe }) = do
  { eventSource: EventSource source@{ cancel }
  , outcome: { unsubscribe }
  } <- flip makeEventSource filter \registerHandler ->
    subscribe $ registerHandler <<< _.event
  pure $ EventSource source
    { cancel = cancel *> unsubscribe }

makeEventSource
  :: forall a b c
   . ((a -> Effect Unit) -> Effect c)
  -> (a -> Maybe b)
  -> Effect { eventSource :: EventSource b, outcome :: c }
makeEventSource subscribeOnEvents filter = do
  { isCanceled: getIsCanceled, cancel } <- makeCanceler
  handlers <- Ref.new $ Map.fromFoldable []
  let
    subscribe handler = do
      id <- uniqueId "sub"
      let unsubscribe = Ref.modify_ (Map.delete id) handlers
      _ <- Ref.modify_ (Map.insert id \event -> handler { unsubscribe, event })
        handlers
      pure { unsubscribe }

  outcome <- subscribeOnEvents \a ->
    case filter a of
      Just b -> do
        isCanceled <- getIsCanceled
        unless isCanceled do
          Ref.read handlers >>= traverse_ (_ $ b)
      Nothing -> pure unit
  pure
    { eventSource: EventSource { cancel, subscribe }
    , outcome
    }

makeCanceler :: Effect { cancel :: Effect Unit, isCanceled :: Effect Boolean }
makeCanceler = do
  isCanceledRef <- Ref.new false
  let
    cancel = Ref.write false isCanceledRef
    isCanceled = Ref.read isCanceledRef
  pure { isCanceled, cancel }

cleanupOnExit :: Ref (Array (Aff Unit)) -> Aff (Aff.Fiber Unit)
cleanupOnExit cleanupRef = Aff.forkAff do
  waitForBeforeExit
  log "Running cleanup on beforeExit"
  runCleanup cleanupRef

addCleanup :: Ref (Array (Aff Unit)) -> Aff Unit -> Effect Unit
addCleanup = map void <<< flip (Ref.modify <<< Array.cons <<< reportError)
  where
  reportError action = do
    try action >>= either
      (log <<< append "[addCleanup][error]: " <<< message)
      (const $ pure unit)

-- | Just as a bracket but without the body.
after :: forall a. Aff a -> (a -> Aff Unit) -> Aff a
after first second = Aff.bracket first second pure