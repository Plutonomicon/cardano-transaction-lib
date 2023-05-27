module Ctl.Internal.Plutip.Services where

import Prelude

import Contract.Prelude (undefined)
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Ctl.Internal.Plutip.Spawn
  ( ManagedProcess
  , OnSignalRef
  , removeOnSignal
  , spawn
  , stop
  )
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , PlutipConfig
  , PostgresConfig
  , PrivateKeyResponse(PrivateKeyResponse)
  , ProcessType(Spawn, Exec)
  , Service(..)
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  )
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (Aff, Milliseconds(Milliseconds))
import Effect.Aff (bracket) as Aff
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.ChildProcess
  ( ChildProcess
  , defaultExecOptions
  , defaultSpawnOptions
  , exec
  )

runServices :: Array Service -> Ref (Array (Aff Unit)) -> Aff Unit
runServices services cleanupRef = traverse_ op services
  where
  op (Service { processType, command, arguments, port }) =
    case processType of
      Exec -> void $ runExecService command arguments
      Spawn -> case port of
        Nothing -> void $ runSpawnService' command arguments
        Just port' -> runSpawnService port' command arguments cleanupRef

runSpawnService
  :: UInt
  -> String
  -> Array (String /\ String)
  -> Ref (Array (Aff Unit))
  -> Aff Unit
runSpawnService port command arguments cleanupRef =
  bracket
    (runSpawnService' command arguments)
    (stopChildProcessWithPort port)
    (const $ pure unit)
    cleanupRef

runSpawnService' :: String -> Array (String /\ String) -> Aff ManagedProcess
runSpawnService' command arguments =
  spawn command (inputArgumentsToSpawnArguments arguments) defaultSpawnOptions
    Nothing

inputArgumentsToSpawnArguments :: Array (String /\ String) -> Array String
inputArgumentsToSpawnArguments = Array.concatMap op
  where
  op (flag /\ arg) = [ "--" <> flag, arg ]

runExecService :: String -> Array (String /\ String) -> Aff ChildProcess
runExecService command arguments = liftEffect $ exec
  (inputArgumentsToExecArguments command arguments)
  defaultExecOptions
  (const $ pure unit)

inputArgumentsToExecArguments :: String -> Array (String /\ String) -> String
inputArgumentsToExecArguments command arguments = command <> op arguments
  where
  op args = Array.foldr
    (\(flag /\ arg) acc -> "--" <> flag <> " " <> arg <> " " <> acc)
    ""
    args

-- Similar to `Aff.bracket`, except cleanup is pushed onto a stack to be run
-- later.
bracket
  :: forall (a :: Type) (b :: Type)
   . Aff a
  -> (a -> Aff Unit)
  -> (a -> Aff b)
  -> Ref (Array (Aff Unit))
  -> Aff b
bracket before after action cleanupRef = do
  Aff.bracket
    before
    (\res -> liftEffect $ Ref.modify_ ([ after res ] <> _) cleanupRef)
    action

-- | Kill a process and wait for it to stop listening on a specific port.
stopChildProcessWithPort :: UInt -> ManagedProcess -> Aff Unit
stopChildProcessWithPort port childProcess = do
  stop childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"

stopChildProcessWithPortAndRemoveOnSignal
  :: UInt -> (ManagedProcess /\ String /\ OnSignalRef) -> Aff Unit
stopChildProcessWithPortAndRemoveOnSignal port (childProcess /\ _ /\ sig) = do
  stop $ childProcess
  void $ recovering defaultRetryPolicy ([ \_ _ -> pure true ])
    \_ -> do
      isAvailable <- isPortAvailable port
      unless isAvailable do
        liftEffect $ throw "retry"
  liftEffect $ removeOnSignal sig

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = limitRetriesByCumulativeDelay (Milliseconds 3000.00) $
  constantDelay (Milliseconds 100.0)
