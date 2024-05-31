module Ctl.Internal.Plutip.Utils
  ( mkDirIfNotExists
  , runCleanup
  , tmpdir
  , cleanupOnExit
  , waitForLine
  )
  where

import Contract.Prelude

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

waitForLine :: forall a. Readable a -> (String -> Effect Unit) -> Effect Aff.Canceler
waitForLine readable handler = do
  isCanceledRef <- Ref.new false
  let cancel = Ref.write false isCanceledRef
  interface <- RL.createInterface readable mempty
  flip RL.setLineHandler interface \line -> do
    isCanceled <- Ref.read isCanceledRef
    unless isCanceled do
      handler line
  pure $ Aff.Canceler $ const $ liftEffect cancel  

cleanupOnExit :: Ref (Array (Aff Unit)) -> Aff (Aff.Fiber Unit)
cleanupOnExit cleanupRef = Aff.forkAff do
  waitForBeforeExit
  log "Running cleanup on beforeExit"
  runCleanup cleanupRef
