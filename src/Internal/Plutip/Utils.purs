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
  , waitForBeforeExit
  , waitForClose
  , waitForError
  , waitForExit
  , waitForUncaughtException
  , waitUntil
  ) where

import Contract.Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Rec.Class
  ( Step(Done, Loop)
  , tailRecM
  )
import Control.Parallel
  ( parallel
  , sequential
  )
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess(ManagedProcess)
  , OnSignalRef
  , removeOnSignal
  , waitForSignal
  )
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Data.Array as Array
import Data.Map as Map
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)
import Effect.Random (randomInt)
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

foreign import setErrorHandler
  :: RL.Interface -> (Error -> Effect Unit) -> Effect OnSignalRef

foreign import onBeforeExit
  :: Effect Unit -> Effect OnSignalRef

foreign import onExit
  :: (Int -> Effect Unit) -> Effect OnSignalRef

foreign import onUncaughtException
  :: (Error -> Effect Unit) -> Effect OnSignalRef

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
    { cancel } <- withOneShotHandler \{ justOnce } ->
      setCloseHandler interface $ justOnce $ cont $ Right unit
    pure $ Aff.Canceler \err -> liftEffect do
      cancel
      cont $ Left $ appendErrorMessage "waitForClose has been canceled" err

-- | Waits until processe's stdout closes.
-- Assuming this means that process is closed as well. 
waitForError :: ManagedProcess -> Aff Error
waitForError (ManagedProcess _ child _) = do
  interface <- liftEffect
    $ flip RL.createInterface mempty
    $ Node.ChildProcess.stdout child
  Aff.makeAff \cont -> do
    { cancel } <- withOneShotHandler \{ justOnce } ->
      setErrorHandler interface \err -> justOnce $ cont $ Right err
    pure $ Aff.Canceler \err -> liftEffect do
      cancel
      cont $ Left $ appendErrorMessage "waitForClose has been canceled" err

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
  log "Cleaning up"
  cleanups <- liftEffect do
    cleanups <- Ref.read cleanupRef
    Ref.write [] cleanupRef
    pure cleanups
  if null cleanups then log "No cleanup needed"
  else do
    sequence_ $ suppressAndLogErrors "runCleanup" <$> cleanups
    log "Cleanup finished"

waitForBeforeExit :: Aff Unit
waitForBeforeExit = Aff.makeAff \cont -> do
  { cancel } <- withOneShotHandler \{ justOnce } -> onBeforeExit $ justOnce do
    log "ON BEFORE EXIT"
    cont $ Right unit
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForBeforeExit" $ cont $ Left err

-- | Specifically for nodejs handlers:
-- Makes sure that the callback is called at most once, and unregistering it
-- on cancelation and on the first call.
withOneShotHandler
  :: ({ justOnce :: Effect Unit -> Effect Unit } -> Effect OnSignalRef)
  -> Effect { cancel :: Effect Unit }
withOneShotHandler with = do
  removeHandler <- Ref.new mempty
  isClosedRef <- Ref.new false
  let
    cancel = do
      join $ Ref.read removeHandler
      Ref.write true isClosedRef
  handle <- with
    { justOnce: \oneShotHandler -> do
        -- otherwise it may be triggered multiple times, for unknown reason
        Ref.read isClosedRef >>= flip unless do
          cancel
          oneShotHandler
    }
  Ref.write (removeOnSignal handle) removeHandler
  pure { cancel }

waitForUncaughtException :: Aff Error
waitForUncaughtException = Aff.makeAff \cont -> do
  n <- randomInt 0 100
  { cancel } <- withOneShotHandler \{ justOnce } ->
    onUncaughtException \err -> justOnce do
      log $ "ON UNCAUGHT EXCEPTION " <> show n
      cont $ Right err
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForUncaughtException" $ cont $ Left err

waitUntil :: forall a. Milliseconds -> Aff (Maybe a) -> Aff a
waitUntil checkingInterval fa = flip tailRecM unit \_ ->
  fa >>= case _ of
    Nothing -> do
      Aff.delay checkingInterval
      pure $ Loop unit
    Just x -> pure $ Done x

waitForExit :: Aff Int
waitForExit = Aff.makeAff \cont -> do
  { cancel } <- withOneShotHandler \{ justOnce } -> onExit \exitcode -> justOnce
    do
      log "ON EXIT"
      cont $ Right exitcode
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForExit" $ cont $ Left err

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
    subscriptionResult <- subscribe \{ unsubscribe, event } -> do
      unsubscribe
      cont event
    case subscriptionResult of
      Right { unsubscribe } -> pure $ Aff.Canceler \err -> liftEffect do
        unsubscribe
        cont $ Left $ appendErrorMessage "waitForEvent:canceled" err
      Left subError -> do
        suppressAndLogErrors "waitForEvent:badSubscription"
          $ cont
          $ Left
          $ appendErrorMessage "Failed to subscribe" subError
        pure Aff.nonCanceler

onLine
  :: forall a b
   . Readable a
  -> (String -> Maybe b)
  -> Effect (EventSource b)
onLine readable =
  map _.eventSource <<< makeEventSource \{ handle: mainHandler } -> do
    interface <- RL.createInterface readable mempty
    handlers <- Ref.new []
    lineHandler <- setLineHandler interface \x -> do
      void
        $ suppressAndLogErrors "onLine:setLineHandler"
        $ mainHandler
        $ Right x
    let
      cancel = \err -> do
        Ref.read handlers >>= traverse_ (try <<< removeOnSignal)
        void
          $ suppressAndLogErrors "onLine:cancel"
          $ mainHandler
          $ Left err
    closeHandler <- setCloseHandler interface
      $ cancel
      $ error "Line event source has been closed."
    errorHandler <- setErrorHandler interface cancel
    Ref.write [ lineHandler, closeHandler, errorHandler ] handlers
    pure
      { outcome: unit
      , unsubscribe: do
          cancel $ error "Unsubscribed from line event."
      }

-- | Create an event source based on another event source, but
-- with smaller variety of events.
narrowEventSource
  :: forall a b
   . (a -> Maybe b)
  -> EventSource a
  -> Effect (EventSource b)
narrowEventSource filter (EventSource source) = annotateError
  "narrowEventSource"
  do
    { eventSource: new
    , outcome: subscriptionResult -- this goes from the source
    } <- flip makeEventSource filter \{ handle } ->
      do -- this is how new event source subscribe on the source
        source.subscribe (handle <<< _.event) >>= case _ of
          Left err -> pure
            { outcome: Left err -- this is not for makeEventSource
            , unsubscribe: pure unit -- how do 'new' unsubscribe from the 'source'
            }
          Right { unsubscribe: unsubFromSource } -> pure
            { outcome: Right unit -- this is not for makeEventSource
            , unsubscribe: unsubFromSource -- how do 'new' unsubscribe from the 'source'
            }
    liftEither subscriptionResult
    pure new

makeEventSource
  :: forall a b c
   . ( { handle :: Either Error a -> Effect Unit }
       -> Effect { unsubscribe :: Effect Unit, outcome :: c }
     )
  -> (a -> Maybe b)
  -> Effect { eventSource :: EventSource b, outcome :: c }
makeEventSource subscribeOnEvents filter = annotateError "make event source" do
  handlers <- Ref.new $ Map.fromFoldable []
  isCanceled <- Ref.new false
  cancelRef <- Ref.new mempty
  let
    markCanceled = Ref.write true isCanceled
    cancel error = Ref.read cancelRef >>= (_ $ error)
    subscribe handler = do
      Ref.read isCanceled >>=
        if _ then
          pure $ Left $ error "Event source is closed."
        else do
          id <- uniqueId "sub"
          let unsubscribe = Ref.modify_ (Map.delete id) handlers
          _ <- Ref.modify_
            (Map.insert id \event -> handler { unsubscribe, event })
            handlers
          pure $ Right { unsubscribe }

  { unsubscribe, outcome } <- subscribeOnEvents
    { handle: \ea -> case ea of
        Left error -> cancel error
        Right a -> case filter a of
          Just b -> do
            Ref.read handlers >>= traverse_ (_ $ Right b)
          Nothing -> pure unit
    }
  flip Ref.write cancelRef \error -> do
    Ref.write mempty cancelRef -- canceler may be called only once
    unsubscribe
    markCanceled
    Ref.read handlers >>= traverse_ \cont ->
      void $ suppressAndLogErrors "makeEventSource:cancel" $ cont $ Left error
    Ref.write (Map.fromFoldable []) handlers

  pure
    { eventSource: EventSource { cancel, subscribe }
    , outcome
    }

cleanupOnExit
  :: Ref (Array (Aff Unit))
  -> Aff { fiber :: Aff.Fiber Unit }
cleanupOnExit cleanupRef = do
  log "Cleanup scheduled"
  let
    handle handlers = do
      handler <- sequential do
        ( handlers.onExit
            <$> parallel waitForExit
        )
          <|>
            ( handlers.onUncaughtException
                <$> parallel waitForUncaughtException
            )
          <|>
            ( handlers.onBeforeExit
                <$ parallel waitForBeforeExit
            )
          <|>
            ( handlers.onWaitForSignal
                <$ parallel (waitForSignal SIGINT)
            )
      handler
    cleanup triggeredBy = do
      log $ "Running cleanup on " <> triggeredBy
      runCleanup cleanupRef

  fiber <- Aff.forkAff $ handle
    { onExit: \code -> cleanup $ "exit with " <> show code
    , onUncaughtException: \err -> do
        cleanup "uncaught exception"
        log $ "Failing irrecoverably after the cleanup after error: " <> show
          err
        liftEffect $ Process.exit 7 -- Failing irrecoverably
    , onBeforeExit: cleanup "before exit"
    , onWaitForSignal: cleanup "SIGINT"
    }
  pure { fiber }

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
  catchError action $ throwError <<< appendErrorMessage withPrefix

appendErrorMessage
  :: String
  -> Error
  -> Error
appendErrorMessage withPrefix =
  error <<< append (withPrefix <> ": ") <<< message
