module Ctl.Internal.Test.E2E.Route
  ( E2ETestName
  , E2EConfigName
  , route
  , parseRoute
  , Route
  , addLinks
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types.PrivateKey (PrivateKey)
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.RawBytes (RawBytes(RawBytes))
import Contract.Config (ContractParams)
import Contract.Monad (Contract, runContract)
import Contract.Test.Cip30Mock (withCip30Mock)
import Contract.Wallet
  ( PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  )
import Contract.Wallet.Key (privateKeysToKeyWallet)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Contract.QueryBackend (mkCtlBackendParams)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.QueryM (ClusterSetup)
import Ctl.Internal.Test.E2E.Feedback.Browser (getClusterSetupRepeatedly)
import Ctl.Internal.Test.E2E.Feedback.Hooks (addE2EFeedbackHooks)
import Ctl.Internal.Wallet.Spec (WalletSpec(ConnectToGenericCip30))
import Data.Array (last)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(Left), note)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (wrap)
import Data.String (stripPrefix)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)

-- | A name of some particular test. Used in the URL
type E2ETestName = String

-- | A name of some particular E2E test environment (`ContractParams` and possible
-- | CIP-30 mock). Used in the URL for routing
type E2EConfigName = String

-- | Router state - parsed from URL by `parseRoute`
type Route =
  { configName :: E2EConfigName
  , testName :: E2ETestName
  , paymentKey :: Maybe PrivatePaymentKey
  , stakeKey :: Maybe PrivateStakeKey
  }

parseRoute :: String -> Either String Route
parseRoute queryString =
  case split (Pattern ":") queryString of
    [ configName, testName ] -> do
      pure { configName, testName, paymentKey: Nothing, stakeKey: Nothing }
    [ configName, testName, paymentKeyStr ] -> do
      paymentKey <- mkPaymentKey paymentKeyStr
      pure
        { configName
        , testName
        , paymentKey: Just paymentKey
        , stakeKey: Nothing
        }
    [ configName, testName, paymentKeyStr, stakeKeyStr ] -> do
      paymentKey <- mkPaymentKey paymentKeyStr
      stakeKey <- mkStakeKey stakeKeyStr
      pure
        { configName
        , testName
        , paymentKey: Just paymentKey
        , stakeKey: Just stakeKey
        }
    _ -> Left "Unable to decode URL"
  where
  mkPrivateKey' :: String -> Maybe PrivateKey
  mkPrivateKey' str = hexToByteArray str >>= RawBytes >>>
    PrivateKey.fromRawBytes

  mkPrivateKey :: String -> Maybe PrivateKey
  mkPrivateKey str =
    mkPrivateKey' str <|> (stripPrefix (Pattern "5820") str >>= mkPrivateKey)

  mkPaymentKey :: String -> Either String PrivatePaymentKey
  mkPaymentKey str = note ("Unable to decode a Payment Key from " <> show str) $
    mkPrivateKey str <#> PrivatePaymentKey

  mkStakeKey :: String -> Either String PrivateStakeKey
  mkStakeKey str = note ("Unable to decode a Stake Key from " <> show str) do
    mkPrivateKey str <#> PrivateStakeKey

addLinks
  :: Map E2EConfigName (ContractParams /\ Maybe String)
  -> Map E2ETestName (Contract Unit)
  -> Effect Unit
addLinks configMaps testMaps = do
  let
    configs = Array.fromFoldable $ Map.keys configMaps
    tests = Array.fromFoldable $ Map.keys testMaps
  _addLinks configs tests
  _setupBlockfrostApi

-- | Serves given examples conditionally, depending on the URL query part.
-- |
-- | The structure of the query string should be one of:
-- | ```
-- | E2EConfigName:E2ETestName
-- | E2EConfigName:E2ETestName:PrivatePaymentKeyCborHex
-- | E2EConfigName:E2ETestName:PrivatePaymentKeyCborHex:PrivateStakeKeyCborHex
-- | ```
-- |
-- | Examples:
-- | ```
-- | gero:MintsMultiple
-- | eternl:AlwaysSucceeds:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209
-- | nami-mock:MintsMultiple:58200b07c066ba037344acee5431e6df41f6034bf1c5ffd6f803751e356807c6a209:5820f0db841df6c7fbc4506c58fad6676db0354a02dfd26efca445715a8adeabc338
-- | ```
-- |
-- | In case that for the specified `E2EConfigName` a `WalletMock` is provided
-- | (present in the `Map`), but no private keys are given, this function
-- | assumes that the test is running on a local cluster, and pre-funded keys
-- | from the cluster should be used. If there's no local cluster, an error
-- | will be thrown.
route
  :: Map E2EConfigName (ContractParams /\ Maybe String)
  -> Map E2ETestName (Contract Unit)
  -> Effect Unit
route configs tests = do
  queryString <- fold <<< last <<< split (Pattern "?") <$> _queryString
  { configName, testName, paymentKey: mbPaymentKey, stakeKey: mbStakeKey } <-
    liftEither $ lmap error $ parseRoute queryString
  config /\ mbMock <-
    liftMaybe
      (error $ noConfigParametersError configName) $
      Map.lookup configName configs
  test <-
    liftMaybe (error $ "Unable to look up the `Contract` to run: " <> testName)
      $ Map.lookup testName tests
  launchAff_ do
    delayIfEternl config
    case mbMock of
      Nothing ->
        runContract config { hooks = addE2EFeedbackHooks config.hooks } test
      Just mock -> do
        { paymentKey, stakeKey } /\ mbClusterSetup <- case mbPaymentKey of
          Just paymentKey ->
            pure $ { paymentKey, stakeKey: mbStakeKey } /\ Nothing
          Nothing -> do
            clusterSetup@{ keys: { payment, stake } } <-
              getClusterSetupRepeatedly
            when (config.networkId /= TestnetId) do
              liftEffect $ throw wrongNetworkIdOnCluster
            pure $ { paymentKey: payment, stakeKey: stake } /\ Just clusterSetup
        let
          configWithHooks =
            config
              -- add hooks that allow communicating between nodejs and the
              -- browser
              { walletSpec = Nothing, hooks = addE2EFeedbackHooks config.hooks }
              -- override service ports from cluster setup if it's present
              # maybe identity setClusterOptions mbClusterSetup
        do
          runContract configWithHooks
            $ withCip30Mock
                (privateKeysToKeyWallet paymentKey stakeKey Nothing) -- FIXME
                mock
                test
  where
  -- Eternl does not initialize instantly. We have to add a small delay.
  delayIfEternl :: ContractParams -> Aff Unit
  delayIfEternl config =
    case config.walletSpec of
      Just (ConnectToGenericCip30 "eternl" _) ->
        delay $ wrap 3000.0
      _ -> pure unit

  noConfigParametersError :: E2EConfigName -> String
  noConfigParametersError configName =
    "Unable to look up the config parameters: " <> configName
      <> "Common reasons are:\n"
      <> "- The page that is used to serve the test contracts is not up to "
      <> "date\n"
      <> "- The name of the test suite configuration is wrong ("
      <> configName
      <> ")"

  wrongNetworkIdOnCluster :: String
  wrongNetworkIdOnCluster =
    "No payment keys were specified, which implies they should be retrieved "
      <> "from a local cluster, however, network ID was set to MainnetId, "
      <>
        "which is incompatible with cardano-testnet, that always uses TestnetId."

  -- Override config values with parameters from cluster setup
  setClusterOptions
    :: ClusterSetup -> ContractParams -> ContractParams
  setClusterOptions
    { ogmiosConfig
    , kupoConfig
    }
    config =
    config
      { backendParams = mkCtlBackendParams
          { ogmiosConfig: ogmiosConfig
          , kupoConfig: kupoConfig
          }
      }

foreign import _queryString :: Effect String

foreign import _writeExampleHTML :: String -> Array String -> Effect Unit

foreign import _addLinks
  :: Array E2EConfigName -> Array E2ETestName -> Effect Unit

foreign import _setupBlockfrostApi :: Effect Unit
