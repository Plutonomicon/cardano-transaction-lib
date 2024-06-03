module Ctl.Internal.Plutip.Utils
  ( mkDirIfNotExists
  , runCleanup
  , tmpdir
  , annotateError
  , cleanupOnExit
  , EventSource(EventSource)
  , onLine
  , makeEventSource
  , narrowEventSource
  , waitForEvent
  , addCleanup
  , scheduleCleanup
  , after
  , whenError
  , suppressAndLogErrors
  , tryAndLogErrors
  , waitForClose
  ) where

import Contract.Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess(..)
  , OnSignalRef
  , makeAffCanceler
  , removeOnSignal
  , waitForSignal
  )
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Data.Array as Array
import Data.Map as Map
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.ChildProcess as Node.ChildProcess
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import Node.ReadLine as RL
import Node.Stream (Readable)

-- TODO: remove this function when PS bindings for os.tmpdir are available.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/726
foreign import tmpdir :: Effect String

foreign import setLineHandler
  :: RL.Interface -> (String -> Effect Unit) -> Effect OnSignalRef

foreign import setCloseHandler
  :: RL.Interface -> Effect Unit -> Effect OnSignalRef

suppressAndLogErrors
  :: forall m. MonadEffect m => MonadError Error m => String -> m Unit -> m Unit
suppressAndLogErrors location = flip catchError $ message
  >>> append ("An error occured and suppressed at " <> location <> ": ")
  >>> log

-- | Waits until processe's stdout closes.
-- Assuming this means that process is closed as well. 
waitForClose :: ManagedProcess -> Aff Unit
waitForClose (ManagedProcess _ child _) = do
  interface <- liftEffect
    $ flip RL.createInterface mempty
    $ Node.ChildProcess.stdout child
  Aff.makeAff \cont -> do
    { canceler, cancelationStatus } <- makeAffCanceler $ const $ pure unit
    _ <- setCloseHandler interface
      $ cancelationStatus
      >>= maybe (Right unit) Left
      >>> cont
    pure canceler

tryAndLogErrors
  :: forall a m
   . MonadEffect m
  => MonadError Error m
  => String
  -> m a
  -> m (Either Error a)
tryAndLogErrors location = try >=> case _ of
  Left err -> do
    log $ "An error occured and suppressed at " <> location <> ": " <> message
      err
    pure $ Left err
  Right a -> pure $ Right a

mkDirIfNotExists :: FilePath -> Effect Unit
mkDirIfNotExists dirName = do
  exists <- FS.exists dirName
  unless exists $ FS.mkdir dirName

runCleanup :: Ref (Array (Aff Unit)) -> Aff Unit
runCleanup cleanupRef = do
  cleanups <- liftEffect $ Ref.read cleanupRef
  sequence_ $ suppressAndLogErrors "runCleanup" <$> cleanups

waitForBeforeExit :: Aff Unit
waitForBeforeExit = Aff.makeAff \cont -> do
  { canceler, cancelationStatus } <- makeAffCanceler $ const $ pure unit
  Process.onBeforeExit
    $ cancelationStatus
    >>= maybe (Right unit) Left
    >>> cont
  pure canceler

newtype EventSource b = EventSource
  { subscribe ::
      ( { unsubscribe :: Effect Unit
        , event :: Either Error b
        }
        -> Effect Unit
      )
      -> Effect (Either Error { unsubscribe :: Effect Unit })
  , cancel :: Error -> Effect Unit
  }

-- | Waits for any event. Note, if the event source throws an async error, any joining process dies.
waitForEvent :: forall a. EventSource a -> Aff a
waitForEvent (EventSource { subscribe }) = annotateError "waitForEvent" $
  Aff.makeAff \cont -> do
    { canceler: Aff.Canceler cancel, cancelationStatus } <-
      makeAffCanceler $ const $ pure unit
    canceler <- makeCanceler
    subscriptionResult <- subscribe \{ unsubscribe, event } -> do
      unsubscribe
      canceler.isCanceled >>= flip when do
        void
          $ suppressAndLogErrors "waitForEvent:event"
          $ cont
          $ Left
          $ error "Action has been canceled"
      cancelationStatus >>= case _ of
        Nothing -> void $ suppressAndLogErrors "waitForEvent:success" $ cont
          event
        Just err -> void $ suppressAndLogErrors "waitForEvent:failure" $ cont $
          Left err
    case subscriptionResult of
      Right { unsubscribe } ->
        pure $ Aff.Canceler \err -> do
          log "canceling"
          liftEffect do
            unsubscribe
            canceler.cancel
          cancel err
      Left subError -> do
        err <- try
          $ annotateError "Failed to subscribe"
          $ throwError subError

        suppressAndLogErrors "waitForEvent:badSubscription"
          $ cont err
        pure $ Aff.Canceler \e -> throwError e

onLine
  :: forall a b
   . Readable a
  -> (String -> Maybe b)
  -> Effect (EventSource b)
onLine readable =
  map _.eventSource <<< makeEventSource \mainHandler -> do
    interface <- RL.createInterface readable mempty
    lineHandler <- setLineHandler interface \x -> do
      void
        $ suppressAndLogErrors "onLine:setLineHandler"
        $ mainHandler
        $ Right x
    setCloseHandler interface do
      removeOnSignal lineHandler
      void
        $ suppressAndLogErrors "onLine:setCloseHandler"
        $ mainHandler
        $ Left
        $ error "Line event source has been closed."

-- | Create an event source based on another event source, but
-- with smaller variety of events.
narrowEventSource
  :: forall a b
   . (a -> Maybe b)
  -> EventSource a
  -> Effect (EventSource b)
narrowEventSource filter (EventSource { subscribe }) = annotateError
  "narrowEventSource"
  do
    { eventSource: EventSource source@{ cancel }
    , outcome: subscriptionResult
    } <- flip makeEventSource filter \registerHandler ->
      subscribe $ registerHandler <<< _.event
    { unsubscribe } <- liftEither subscriptionResult
    pure $ EventSource source
      { cancel = \e -> cancel e *> unsubscribe }

makeEventSource
  :: forall a b c
   . ((Either Error a -> Effect Unit) -> Effect c)
  -> (a -> Maybe b)
  -> Effect { eventSource :: EventSource b, outcome :: c }
makeEventSource subscribeOnEvents filter = annotateError "make event source" do
  handlers <- Ref.new $ Map.fromFoldable []
  { isCanceled, cancel } <- makeErrorCanceler \error -> do
    Ref.read handlers >>= traverse_ \cont ->
      void $ suppressAndLogErrors "makeEventSource:cancel" $ cont $ Left error
    Ref.write (Map.fromFoldable []) handlers
  let
    subscribe handler = do
      isCanceled >>=
        if _ then
          pure $ Left $ error "Event source is closed."
        else do
          id <- uniqueId "sub"
          let unsubscribe = Ref.modify_ (Map.delete id) handlers
          _ <- Ref.modify_
            (Map.insert id \event -> handler { unsubscribe, event })
            handlers
          pure $ Right { unsubscribe }

  outcome <- subscribeOnEvents \ea -> case ea of
    Left error -> cancel error
    Right a -> case filter a of
      Just b -> do
        isCanceled >>= flip unless do
          Ref.read handlers >>= traverse_ (_ $ Right b)
      Nothing -> pure unit
  pure
    { eventSource: EventSource { cancel, subscribe }
    , outcome
    }

makeErrorCanceler
  :: forall e
   . (e -> Effect Unit)
  -> Effect
       { cancel :: e -> Effect Unit
       , isCanceled :: Effect Boolean
       }
makeErrorCanceler customCancelation = do
  isCanceledRef <- Ref.new false
  let
    cancel err = do
      log "Canceled."
      Ref.write false isCanceledRef
      customCancelation err
    isCanceled = Ref.read isCanceledRef
  pure { isCanceled, cancel }

makeCanceler :: Effect { cancel :: Effect Unit, isCanceled :: Effect Boolean }
makeCanceler = do
  isCanceledRef <- Ref.new false
  let
    cancel = Ref.write false isCanceledRef
    isCanceled = Ref.read isCanceledRef
  pure { isCanceled, cancel }

cleanupOnExit
  :: Ref (Array (Aff Unit))
  -> Aff
       { onBeforeExit :: Aff.Fiber Unit
       , onSigint :: Aff.Fiber Unit
       }
cleanupOnExit cleanupRef = do
  triggered <- AVar.empty
  onBeforeExit <- Aff.forkAff do
    waitForBeforeExit
    canStart <- AVar.tryPut unit triggered
    when canStart do
      log "Running cleanup on exit"
      runCleanup cleanupRef
  onSigint <- Aff.forkAff do
    waitForSignal SIGINT
    canStart <- AVar.tryPut unit triggered
    when canStart do
      log "Running cleanup on SIGINT"
      runCleanup cleanupRef
  pure { onBeforeExit, onSigint }

addCleanup :: Ref (Array (Aff Unit)) -> Aff Unit -> Effect Unit
addCleanup = map void <<< flip
  (Ref.modify <<< Array.cons <<< suppressAndLogErrors "[addCleanup][error]: ")

scheduleCleanup
  :: forall a
   . Ref (Array (Aff Unit))
  -> Aff a
  -> (a -> Aff Unit)
  -> Aff a
scheduleCleanup cleanupRef create cleanup =
  after create $ liftEffect <<< addCleanup cleanupRef <<< cleanup

-- Similar to `catchError` but preserves the error
whenError :: forall (a :: Type). Aff Unit -> Aff a -> Aff a
whenError whenErrorAction action = do
  res <- try action
  when (isLeft res) whenErrorAction
  liftEither res

-- | Just as a bracket but without the body.
after :: forall a. Aff a -> (a -> Aff Unit) -> Aff a
after first second = Aff.bracket first second pure

annotateError
  :: forall (a :: Type) m
   . MonadError Error m
  => String
  -> m a
  -> m a
annotateError withPrefix action =
  catchError action \err ->
    throwError $ error $ withPrefix <> ": " <> message err
