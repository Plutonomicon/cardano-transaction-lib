-- | Running Plutip test plans with Blockfrost
module Contract.Test.Blockfrost
  ( BlockfrostKeySetup
  , runContractTestsWithBlockfrost
  , executeContractTestsWithBlockfrost
  ) where

import Prelude

import Contract.Config
  ( BlockfrostBackendParams
  , ContractParams
  , CtlBackendParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , PrivateStakeKeySource(PrivateStakeKeyFile)
  , QueryBackendParams(BlockfrostBackendParams)
  , ServerConfig
  , WalletSpec(UseKeys)
  )
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.E2E.Runner (readBoolean)
import Ctl.Internal.Test.KeyDir (runContractTestsWithKeyDir)
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.Number as Number
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error, throw)
import Node.Process (lookupEnv)
import Test.Spec.Runner (Config)

-- | All parameters that are needed to run Contract tests using
-- | Blockfrost API.
-- |
-- | Includes:
-- |
-- | - Private payment and (optionally) stake keys
-- | - A directory to store temporary private keys that will be used in tests.
-- |   In case of a sudden test interruption, funds will not be lost because
-- |   the private keys will be saved to files.
type BlockfrostKeySetup =
  { privateKeySources ::
      { payment :: PrivatePaymentKeySource
      , stake :: Maybe PrivateStakeKeySource
      }
  , testKeysDirectory :: String
  }

-- | A function that interprets a Plutip test plan into an `Aff`, given a
-- | pre-funded address and a Blockfrost API endpoint.
-- |
-- | Accepts:
-- |
-- | 1. Runtime parameters for `Contract`
-- | 2. Parameters for Blockfrost backend
-- | 3. Optional parameters for CTL backend if it should be used
-- | 4. Key setup parameters - keys are used to provide funds to the test suite.
-- |    Create the keys using [this guide](https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/)
-- |    and fund them using the test ADA faucet: https://docs.cardano.org/cardano-testnet/tools/faucet
-- | 5. A test suite to run.
-- |
-- | Note that this function does not start a Plutip cluster. Instead, it
-- | substitutes it with Blockfrost.
-- |
-- | **If you are using a paid Blockfrost plan**, be careful with what you run with
-- | this function.
-- |
-- | Avoid moving the funds around too much using `withWallets`
-- | in the tests to save on both time and costs.
runContractTestsWithBlockfrost
  :: ContractParams
  -> BlockfrostBackendParams
  -> Maybe CtlBackendParams
  -> BlockfrostKeySetup
  -> TestPlanM ContractTest Unit
  -> TestPlanM (Aff Unit) Unit
runContractTestsWithBlockfrost
  contractParams
  backendParams
  mbCtlBackendParams
  { privateKeySources, testKeysDirectory }
  suite =
  runContractTestsWithKeyDir
    config
    testKeysDirectory
    suite
  where
  config =
    contractParams
      { backendParams = BlockfrostBackendParams backendParams mbCtlBackendParams
      , walletSpec = Just $ UseKeys privateKeySources.payment
          privateKeySources.stake
      }

-- | Reads environment variables containing Blockfrost test suite configuration
-- | parameters and runs a given test suite using
-- | `runContractTestsWithBlockfrost`.
executeContractTestsWithBlockfrost
  :: Config
  -> ContractParams
  -> Maybe CtlBackendParams
  -> TestPlanM ContractTest Unit
  -> Aff Unit
executeContractTestsWithBlockfrost
  testConfig
  contractParams
  mbCtlBackendParams
  suite = do
  blockfrostApiKey <- liftEffect $
    lookupEnv "BLOCKFROST_API_KEY" <#> notEmptyString
  when (isNothing blockfrostApiKey) do
    liftEffect $ Console.warn $
      "Warning: BLOCKFROST_API_KEY is not set. " <>
        "If you are using a public instance, the tests will fail"
  privatePaymentKeyFile <-
    getEnvVariable "PRIVATE_PAYMENT_KEY_FILE"
      "Please specify a payment key file"
  mbPrivateStakeKeyFile <- liftEffect $
    lookupEnv "PRIVATE_STAKE_KEY_FILE" <#> notEmptyString
  confirmTxDelay <- liftEffect $
    lookupEnv "TX_CONFIRMATION_DELAY_SECONDS" >>= parseConfirmationDelay
  when (confirmTxDelay < Just (Seconds 20.0)) do
    liftEffect $ Console.warn $
      "Warning: It is recommended to set TX_CONFIRMATION_DELAY_SECONDS to at "
        <> "least 20 seconds to let the changes propagate after transaction "
        <> "submission."
  testKeysDirectory <- getEnvVariable "BACKUP_KEYS_DIR"
    "Please specify a directory to store temporary private keys in"
  blockfrostConfig <- liftEffect $ readBlockfrostServerConfig
  let
    backendParams =
      { blockfrostConfig
      , blockfrostApiKey
      , confirmTxDelay
      }
  interpretWithConfig testConfig $
    runContractTestsWithBlockfrost contractParams backendParams
      mbCtlBackendParams
      { privateKeySources:
          { payment: PrivatePaymentKeyFile privatePaymentKeyFile
          , stake: PrivateStakeKeyFile <$> mbPrivateStakeKeyFile
          }
      , testKeysDirectory
      }
      suite
  where
  getEnvVariable :: String -> String -> Aff String
  getEnvVariable variable text = liftEffect do
    res <- notEmptyString <$> lookupEnv variable >>= case _ of
      Nothing -> throw $ text <> " (" <> variable <> ")"
      Just result -> pure result
    pure res

  -- Treat env variables set to "" as empty
  notEmptyString :: Maybe String -> Maybe String
  notEmptyString =
    case _ of
      Just "" -> Nothing
      other -> other

  parseConfirmationDelay :: Maybe String -> Effect (Maybe Seconds)
  parseConfirmationDelay =
    notEmptyString >>> maybe (pure Nothing) \str ->
      case Number.fromString str of
        Nothing -> liftEffect $ throw
          "TX_CONFIRMATION_DELAY_SECONDS must be set to a valid number"
        Just number -> pure $ Just $ Seconds number

readBlockfrostServerConfig :: Effect ServerConfig
readBlockfrostServerConfig = do
  port <- lookupEnv "BLOCKFROST_PORT" >>= \mbPort ->
    liftMaybe (error "Unable to read BLOCKFROST_PORT environment variable")
      (mbPort >>= UInt.fromString)
  host <- lookupEnv "BLOCKFROST_HOST" >>=
    liftMaybe (error "Unable to read BLOCKFROST_HOST")
  secure <- lookupEnv "BLOCKFROST_SECURE" >>= \mbSecure ->
    liftMaybe
      ( error
          "Unable to read BLOCKFROST_SECURE ('true' - use HTTPS, 'false' - use HTTP)"
      )
      (mbSecure >>= readBoolean)
  path <- lookupEnv "BLOCKFROST_PATH"
  pure { port, host, secure, path }
