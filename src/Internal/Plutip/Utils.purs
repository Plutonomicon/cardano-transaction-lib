module Ctl.Internal.Plutip.Utils
  ( mkDirIfNotExists
  , runCleanup
  , tmpdir
  , cleanupOnExit
  , handleLines
  , EventSource(EventSource)
  , onLine
  , waitForEvent
  ) where

import Contract.Prelude

import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff as Aff
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
onLine readable filterLine = do
  { isCanceled: getIsCanceled, cancel } <- makeCanceler
  handlers <- Ref.new $ Map.fromFoldable []
  let
    subscribe handler = do
      id <- uniqueId "sub"
      let unsubscribe = Ref.modify_ (Map.delete id) handlers
      _ <- Ref.modify_ (Map.insert id \event -> handler { unsubscribe, event })
        handlers
      pure { unsubscribe }

  interface <- RL.createInterface readable mempty
  flip RL.setLineHandler interface \line ->
    case filterLine line of
      Just a -> do
        isCanceled <- getIsCanceled
        unless isCanceled do
          Ref.read handlers >>= traverse_ (_ $ a)
      Nothing -> pure unit
  pure $ EventSource { cancel, subscribe }

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
