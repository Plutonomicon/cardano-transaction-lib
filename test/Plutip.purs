module Test.Ctl.Plutip
  ( main
  ) where

import Contract.Prelude

import Cardano.Types.Address as Cardano.Types
import Contract.Config as Config
import Contract.Monad (Contract, launchAff_, withContractEnv)
import Contract.Monad as Contract
import Contract.Prelude
  ( Either(..)
  , Maybe(..)
  , for
  , for_
  , intercalate
  , isJust
  , length
  , liftAff
  , liftEffect
  , log
  , maybe
  , traverse
  )
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Staking (getPoolIds)
import Contract.Test.Plutip (PlutipConfig, defaultPlutipConfig, noWallet)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Contract.TextEnvelope
  ( TextEnvelope(..)
  , TextEnvelopeType(..)
  , decodeTextEnvelope
  )
import Contract.Wallet
  ( getWalletAddresses
  , getWalletBalance
  , mkKeyWalletFromPrivateKeys
  , ownPaymentPubKeyHashes
  , withKeyWallet
  )
import Contract.Wallet.KeyFile
  ( mkKeyWalletFromFiles
  , privatePaymentKeyFromTextEnvelope
  )
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Spawn (ManagedProcess(..), stop)
import Ctl.Internal.Plutip.Types (StopClusterResponse(StopClusterSuccess))
import Ctl.Internal.Plutip.Utils
  ( cleanupOnExit
  , onLine
  , tmpdir
  , waitForBeforeExit
  , waitForExit
  , waitForUncaughtException
  )
import Ctl.Internal.Testnet.Contract as Testnet.Contract
import Ctl.Internal.Testnet.Server
  ( checkTestnet
  , redirectChannels
  , runTestnetTestPlan
  , startTestnet
  , stopTestnet
  , testTestnetContracts
  )
import Ctl.Internal.Testnet.Server as Testnet
import Ctl.Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , Event(..)
  , LoggingFormat(..)
  )
import Ctl.Internal.Testnet.Types as Testnet.Types
import Ctl.Internal.Testnet.Utils (findTestnetPaths, onTestnetEvent, waitFor)
import Ctl.Internal.Types.ScriptLookups (ownPaymentPubKeyHash)
import Ctl.Internal.Wallet.KeyFile (privateStakeKeyFromTextEnvelope)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Milliseconds(Milliseconds)
  , bracket
  , cancelWith
  , delay
  , effectCanceler
  , forkAff
  , killFiber
  , launchAff
  )
import Effect.Aff as Aff
import Effect.Exception (Error, error, throw)
import Effect.Ref as Ref
import Mote (group, test)
import Mote.Monad (mapTest)
import Mote.TestPlanM (TestPlanM)
import Mote.TestPlanM as Utils
import Node.Buffer as Node.Buffer
import Node.ChildProcess (kill)
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream as Node.Stream
import Record as Record
import Test.Ctl.BalanceTx.ChangeGeneration as ChangeGeneration
import Test.Ctl.Plutip.Common (config)
import Test.Ctl.Plutip.Contract as Contract
import Test.Ctl.Plutip.Contract.Assert as Assert
import Test.Ctl.Plutip.Contract.ClusterParameters as ClusterParameters
import Test.Ctl.Plutip.Contract.Mnemonics as Mnemonics
import Test.Ctl.Plutip.Contract.OgmiosMempool as OgmiosMempool
import Test.Ctl.Plutip.ExUnits as ExUnits
import Test.Ctl.Plutip.Logging as Logging
import Test.Ctl.Plutip.SameWallets as SameWallets
import Test.Ctl.Plutip.UtxoDistribution as UtxoDistribution
import Test.Ctl.QueryM.AffInterface as QueryM.AffInterface
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

foreign import readableFromBuffer
  :: Node.Buffer.Buffer -> Effect (Node.Stream.Readable ())

readGenesisPkey :: FilePath -> Aff Config.PrivatePaymentKey
readGenesisPkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode genesis pkey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode genesis pkey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

readGenesisStakingPkey :: FilePath -> Aff Config.PrivateStakeKey
readGenesisStakingPkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode genesis pkey envelope")
      <<< decodeTextEnvelope
      =<< readTextFile UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = StakeSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode genesis pkey from decoded envelope")
    $ privateStakeKeyFromTextEnvelope envelope'

askAddressFunds
  :: { socketPath :: FilePath
     , testnetMagic :: Int
     }
  -> Cardano.Types.Address
  -> Aff Unit
askAddressFunds { socketPath, testnetMagic } address = do
  channels <- do
    { stderr, stdout } <- execCardanoCli
      [ "query"
      , "utxo"
      , "--socket-path"
      , socketPath
      , "--testnet-magic"
      , show testnetMagic
      , "--address"
      , Cardano.Types.toBech32 address
      ]
    liftEffect
      $ { stderr: _, stdout: _ }
      <$> onLine stderr Just
      <*> onLine stdout Just

  void $ redirectChannels channels
    { stderrTo:
        { console: Just "[cardano-cli][stderr]"
        , log: Just
            "/home/alexey/cardano-transaction-lib/cardano-cli.stderr.log"
        }
    , stdoutTo:
        { console: Just "[cardano-cli][stdout]"
        , log: Just
            "/home/alexey/cardano-transaction-lib/cardano-cli.stdout.log"
        }
    }

defaultStartupParams :: { | CardanoTestnetStartupParams () }
defaultStartupParams =
  ( Testnet.Types.defaultStartupParams
      { testnetMagic: 2 }
  )
    { nodeLoggingFormat = Just Testnet.Types.LogAsJson }

-- Run with `npm run plutip-test`
main :: Effect Unit
main = (interruptOnSignal SIGINT =<< _) $ launchAff $ void do

  -- cleanupRef <- liftEffect $ Ref.new []
  -- _ <- cleanupOnExit cleanupRef
  -- { paths, testnet: { process } } <-
  --   Testnet.startTestnetCluster
  --     Testnet.defaultStartupParams
  --     cleanupRef
  --     { kupoConfig: defaultPlutipConfig.kupoConfig
  --     , ogmiosConfig: defaultPlutipConfig.ogmiosConfig
  --     }
  -- genesisPaths <- liftEffect
  --   $ readGenesisKeyPaths {workdir: paths.testnetDirectory}
  -- for genesisPaths $ readGenesisPkey <<< _.skey
  -- log $ show $ "Parsed genesis keys: " <> show (isJust key)
  --
  -- _ <- Testnet.Contract.runContract defaultPlutipConfig do
  -- key <-
  --   liftAff $ readGenesisPkey $ genesis1Path paths.testnetDirectory
  -- let spo2 = mkKeyWalletFromPrivateKeys key Nothing

  _ <- Testnet.Contract.withContractEnv defaultPlutipConfig \cluster env ->
    Contract.runContractInEnv env do
      let
        Testnet.MkStartedTestnetCluster { paths } = cluster
        addressesDir = paths.testnetDirectory <</>> "addresses"

      wallet <- liftAff $ mkKeyWalletFromFiles
        (addressesDir <</>> "pool-owner1.skey")
        (Just $ addressesDir <</>> "pool-owner1-stake.skey")
      _ <- withKeyWallet wallet do
        log <<< append "Balance: " <<< show =<< getWalletBalance

      log <<< show =<< getProtocolParameters
      log <<< show =<< getPoolIds

  -- startupFailureWaiting <- onStartupFailure source
  --   (show >>> append "Failed to startup testnet: " >>> error >>> throwError)

  log <<< append "Tmp dir is: " =<< liftEffect tmpdir

-- flip cancelWith (effectCanceler (exitCode 1)) do
--   Utils.interpretWithConfig
--     defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true }
--     $ group "Plutip" do
--         testTestnetContracts config Mnemonics.suite
--         group "ExUnits - normal limits" do
--           testTestnetContracts config $ ExUnits.mkFailingSuite 3000
--           testTestnetContracts config $ ExUnits.mkSuite 2550
--         group "ExUnits - relaxed limits" do
--           testTestnetContracts configWithMaxExUnits $ ExUnits.mkSuite 3000
--         testTestnetContracts config Assert.suite
--         Logging.suite
--         testStartTestnet
--         testTestnetContracts config $ do
--           flip mapTest QueryM.AffInterface.suite
--             (noWallet <<< wrapQueryM)
--           ChangeGeneration.suite
--           Contract.suite
--         UtxoDistribution.suite
--         testTestnetContracts config OgmiosMempool.suite
--         runTestnetTestPlan config SameWallets.suite
--         ClusterParameters.runTest

configWithMaxExUnits :: PlutipConfig
configWithMaxExUnits = config
  { clusterConfig = config.clusterConfig { raiseExUnitsToMax = true } }

-- testStartTestnet:: TestPlanM (Aff Unit) Unit
-- testStartTestnet = group "Server" do
--   test "startTestnet / stopTestnet" do
--     -- bracket (startTestnet config)
--     --   (stopChildProcessWithPort config.port) $ const do
--       checkTestnet config
--       _startRes <- startTestnet config [ [] ]
--       stopRes <- stopTestnet config
--       stopRes `shouldSatisfy` case _ of
--         StopClusterSuccess -> true
--         _ -> false

execCardanoCli
  :: Array String
  -> Aff
       { stdout :: Node.Stream.Readable ()
       , stderr :: Node.Stream.Readable ()
       , process :: Node.ChildProcess.ChildProcess
       }
execCardanoCli params = Aff.makeAff \cont -> do
  processRef <- Ref.new Nothing
  process <- Node.ChildProcess.exec
    ("cardano-cli " <> intercalate " " params)
    Node.ChildProcess.defaultExecOptions
    ( \{ error: err, stderr, stdout } -> do
        process <-
          liftMaybe (error "Couldn't find executed process" :: Error)
            =<< Ref.read processRef
        stderrStream <- readableFromBuffer stderr :: Effect _
        stdoutStream <- readableFromBuffer stdout
        let
          result =
            { stderr: stderrStream
            , stdout: stdoutStream
            , process
            }
        cont $ maybe (Right result) Left err
    )
  Ref.write (Just process) processRef
  pure $ Aff.Canceler \err -> liftEffect $ cont $ Left err