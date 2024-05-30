module Ctl.Internal.Testnet.Server
  ( runTestnetContract
  , withTestnetContractEnv
  , startTestnetCluster
  , stopTestnetCluster
  , startTestnetServer
  , checkTestnetServer
  , stopChildProcessWithPort
  , testTestnetxContracts
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Affjax (defaultRequest) as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as Header
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Contract.Address (NetworkId(MainnetId))
import Contract.Chain (waitNSlots)
import Contract.Config (defaultSynchronizationParams, defaultTimeParams)
import Contract.Monad (Contract, ContractEnv, liftContractM, runContractInEnv)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.State (State, execState, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (censor, execWriterT, tell)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Contract.Hooks (emptyHooks)
import Ctl.Internal.Contract.Monad
  ( buildBackend
  , getLedgerConstants
  , mkQueryHandle
  , stopContractEnv
  )
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Logging (Logger, mkLogger, setupLogs)
-- import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
-- import Ctl.Internal.Plutip.Spawn
--   ( ManagedProcess
--   , NewOutputAction(Success, NoOp)
--   , OnSignalRef
--   , cleanupOnSigint
--   , cleanupTmpDir
--   , removeOnSignal
--   , spawn
--   , stop
--   )
import Ctl.Internal.Plutip.Types
  ( ClusterStartupParameters
  , ClusterStartupRequest(ClusterStartupRequest)
  , PlutipConfig
  , PrivateKeyResponse(PrivateKeyResponse)
  , StartClusterResponse(ClusterStartupSuccess, ClusterStartupFailure)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse
  )
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Service.Error
  ( ClientError(ClientDecodeJsonError, ClientHttpError)
  , pprintClientError
  )
import Ctl.Internal.Test.ContractTest
  ( ContractTest(ContractTest)
  , ContractTestPlan(ContractTestPlan)
  , ContractTestPlanHandler
  )
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.UtxoDistribution
  ( class UtxoDistribution
  , InitialUTxODistribution
  , InitialUTxOs
  , decodeWallets
  , encodeDistribution
  , keyWallets
  , transferFundsFromEnterpriseToBase
  )
import Ctl.Internal.Types.UsedTxOuts (newUsedTxOuts)
import Ctl.Internal.Wallet.Key (PrivatePaymentKey(PrivatePaymentKey))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, isLeft)
import Data.Foldable (sum)
import Data.HTTP.Method as Method
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Set as Set
import Data.String.CodeUnits (indexOf) as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (foldMap, for, for_, sequence_, traverse_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (Aff, Milliseconds(Milliseconds), try)
import Effect.Aff (bracket) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry
  ( RetryPolicy
  , constantDelay
  , limitRetriesByCumulativeDelay
  , recovering
  )
import Effect.Class (liftEffect)
import Effect.Exception (error, message, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import JS.BigInt as BigInt
import Mote (bracket) as Mote
import Mote.Description (Description(Group, Test))
import Mote.Monad (MoteT(MoteT), mapTest)
import Node.ChildProcess (defaultSpawnOptions)
import Node.FS.Sync (exists, mkdir) as FSSync
import Node.Path (FilePath, dirname)
import Type.Prelude (Proxy(Proxy))

-- | Run several `Contract`s in tests in a (single) Testnet environment (cardano-testnet, kupo, etc.).
-- | NOTE: This uses `MoteT`s bracketing, and thus has the same caveats.
-- |       Namely, brackets are run for each of the top-level groups and tests
-- |       inside the bracket.
-- |       If you wish to only set up Testnet once, ensure all tests that are passed
-- |       to `testTestnetContracts` are wrapped in a single group.
testTestnetContracts
  :: TestnetConfig
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
testTestnetContracts testnetCfg tp = do